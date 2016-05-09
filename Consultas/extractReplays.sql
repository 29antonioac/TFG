DROP TABLE result;
CREATE TABLE result (
		ReplayID INT NOT NULL,
        Duration INT NOT NULL,
		Frame INT NOT NULL,
        Minerals INT NOT NULL,
        Gas INT NOT NULL,
        Supply INT NOT NULL,
        TotalMinerals INT NOT NULL,
        TotalGas INT NOT NULL,
        TotalSupply INT NOT NULL,
        GroundUnitValue INT NOT NULL,
        BuildingValue INT NOT NULL,
        AirUnitValue INT NOT NULL,
        EnemyMinerals INT NOT NULL,
        EnemyGas INT NOT NULL,
        EnemySupply INT NOT NULL,
        EnemyTotalMinerals INT NOT NULL,
        EnemyTotalGas INT NOT NULL,
        EnemyTotalSupply INT NOT NULL,
        EnemyGroundUnitValue INT NOT NULL,
        EnemyBuildingValue INT NOT NULL,
        EnemyAirUnitValue INT NOT NULL,
        ResourceValue INT NOT NULL,
        EnemyResourceValue INT NOT NULL,
        Winner INT NOT NULL        
    );
    
DROP PROCEDURE extractReplay;

DELIMITER //

CREATE PROCEDURE extractReplay (IN IDReplay INT)

BEGIN
	DECLARE PlayerA INT;
	DECLARE PlayerB INT;
	DECLARE repDuration INT;

	SELECT A.PlayerReplayID
		,B.PlayerReplayID
	INTO PlayerA
		,PlayerB
	FROM (
		SELECT PlayerReplayID
		FROM playerreplay pr
		WHERE pr.ReplayID = IDReplay
			AND pr.RaceID != 5
		) AS A
	INNER JOIN (
		SELECT PlayerReplayID
		FROM playerreplay pr
		WHERE pr.ReplayID = IDReplay
			AND pr.RaceID != 5
		) AS B
		ON A.PlayerReplayID < B.PlayerReplayID;

	SELECT Duration
	INTO repDuration
	FROM replay
	WHERE replay.ReplayID = IDReplay;

	-- Extract one replay
	INSERT INTO result (
		ReplayID
		,Duration
		,Frame
		,Minerals
		,Gas
		,Supply
		,TotalMinerals
		,TotalGas
		,TotalSupply
		,GroundUnitValue
		,BuildingValue
		,AirUnitValue
		,EnemyMinerals
		,EnemyGas
		,EnemySupply
		,EnemyTotalMinerals
		,EnemyTotalGas
		,EnemyTotalSupply
		,EnemyGroundUnitValue
		,EnemyBuildingValue
		,EnemyAirUnitValue
		,ResourceValue
		,EnemyResourceValue
		,Winner
		)
	SELECT IDReplay
		,repDuration
		,AB.Frame
		,@minerals := IFNULL(Minerals, @minerals) AS Minerals
		,@gas := IFNULL(Gas, @gas) AS Gas
		,@supply := IFNULL(Supply, @supply) AS Supply
		,@totMinerals := IFNULL(TotalMinerals, @totMinerals) AS TotalMinerals
		,@totGas := IFNULL(TotalGas, @totGas) AS TotalGas
		,@totSupply := IFNULL(TotalSupply, @totSupply) AS TotalSupply
		,@ground := IFNULL(GroundUnitValue, @ground) AS GroundUnitValue
		,@building := IFNULL(BuildingValue, @building) AS BuildingValue
		,@air := IFNULL(AirUnitValue, @air) AS AirUnitValue
		,@enemyMinerals := IFNULL(EnemyMinerals, @enemyMinerals) AS EnemyMinerals
		,@enemyGas := IFNULL(EnemyGas, @enemyGas) AS EnemyGas
		,@enemySupply := IFNULL(EnemySupply, @enemySupply) AS EnemySupply
		,@enemyTotMinerals := IFNULL(EnemyTotalMinerals, @enemyTotMinerals) AS EnemyTotalMinerals
		,@enemyTotGas := IFNULL(EnemyTotalGas, @enemyTotGas) AS EnemyTotalGas
		,@enemyTotSupply := IFNULL(EnemyTotalSupply, @enemyTotSupply) AS EnemyTotalSupply
		,@enemyGround := IFNULL(EnemyGroundUnitValue, @enemyGround) AS EnemyGroundUnitValue
		,@enemyBuilding := IFNULL(EnemyBuildingValue, @enemyBuilding) AS EnemyBuildingValue
		,@enemyAir := IFNULL(EnemyAirUnitValue, @enemyAir) AS EnemyAirUnitValue
		,@resourcesA := IFNULL(C.ResourceValue, @resourcesA) AS ResourceValue
		,@resourcesB := IFNULL(D.ResourceValue, @resourcesB) AS EnemyResourceValue
		,@winnerA := IFNULL(WinnerA, @winnerA) AS WinnerA
	FROM (
		SELECT IFNULL(A.Frame, B.Frame) AS Frame
			,A.Minerals
			,A.Gas
			,A.Supply
			,A.TotalMinerals
			,A.TotalGas
			,A.TotalSupply
			,A.Winner AS WinnerA
			,B.Minerals AS EnemyMinerals
			,B.Gas AS EnemyGas
			,B.Supply AS EnemySupply
			,B.TotalMinerals AS EnemyTotalMinerals
			,B.TotalGas AS EnemyTotalGas
			,B.TotalSupply AS EnemyTotalSupply
			,B.Winner AS WinnerB
		FROM (
			SELECT rc.Frame
				,pr.PlayerReplayID
				,rc.Minerals
				,rc.Gas
				,rc.Supply
				,rc.TotalMinerals
				,rc.TotalGas
				,rc.TotalSupply
				,pr.Winner
			FROM resourcechange rc
			INNER JOIN playerreplay pr
				ON pr.PlayerReplayID = rc.PlayerReplayID
			INNER JOIN replay r
				ON r.ReplayID = pr.ReplayID
			WHERE r.ReplayID = IDReplay
				AND pr.PlayerReplayID = PlayerA
			-- AND Frame > 0
			ORDER BY rc.Frame ASC
			) AS A
		LEFT JOIN (
			SELECT rc.Frame
				,pr.PlayerReplayID
				,rc.Minerals
				,rc.Gas
				,rc.Supply
				,rc.TotalMinerals
				,rc.TotalGas
				,rc.TotalSupply
				,pr.Winner
			FROM resourcechange rc
			INNER JOIN playerreplay pr
				ON pr.PlayerReplayID = rc.PlayerReplayID
			INNER JOIN replay r
				ON r.ReplayID = pr.ReplayID
			WHERE r.ReplayID = IDReplay
				AND pr.PlayerReplayID = PlayerB
			-- AND Frame > 0
			ORDER BY rc.Frame ASC
			) AS B
			ON A.Frame = B.Frame
				AND A.PlayerReplayID != B.PlayerReplayID
		
		UNION ALL
		
		SELECT IFNULL(A.Frame, B.Frame) AS Frame
			,A.Minerals
			,A.Gas
			,A.Supply
			,A.TotalMinerals
			,A.TotalGas
			,A.TotalSupply
			,A.Winner AS WinnerA
			,B.Minerals
			,B.Gas
			,B.Supply
			,B.TotalMinerals AS EnemyTotalMinerals
			,B.TotalGas AS EnemyTotalGas
			,B.TotalSupply AS EnemyTotalSupply
			,B.Winner AS WinnerB
		FROM (
			SELECT rc.Frame
				,pr.PlayerReplayID
				,rc.Minerals
				,rc.Gas
				,rc.Supply
				,rc.TotalMinerals
				,rc.TotalGas
				,rc.TotalSupply
				,pr.Winner
			FROM resourcechange rc
			INNER JOIN playerreplay pr
				ON pr.PlayerReplayID = rc.PlayerReplayID
			INNER JOIN replay r
				ON r.ReplayID = pr.ReplayID
			WHERE r.ReplayID = IDReplay
				AND pr.PlayerReplayID = PlayerA
			-- AND Frame > 0
			ORDER BY rc.Frame ASC
			) AS A
		RIGHT JOIN (
			SELECT rc.Frame
				,pr.PlayerReplayID
				,rc.Minerals
				,rc.Gas
				,rc.Supply
				,rc.TotalMinerals
				,rc.TotalGas
				,rc.TotalSupply
				,pr.Winner
			FROM resourcechange rc
			INNER JOIN playerreplay pr
				ON pr.PlayerReplayID = rc.PlayerReplayID
			INNER JOIN replay r
				ON r.ReplayID = pr.ReplayID
			WHERE r.ReplayID = IDReplay
				AND pr.PlayerReplayID = PlayerB
			-- AND Frame > 0
			ORDER BY rc.Frame ASC
			) AS B
			ON A.Frame = B.Frame
				AND A.PlayerReplayID != B.PlayerReplayID
		WHERE A.Frame IS NULL
		) AS AB
	LEFT JOIN (
		SELECT DISTINCT rc.Frame
			,sum(GroundUnitValue) AS GroundUnitValue
			,sum(BuildingValue) AS BuildingValue
			,sum(AirUnitValue) AS AirUnitValue
			,sum(EnemyGroundUnitValue) AS EnemyGroundUnitValue
			,sum(EnemyBuildingValue) AS EnemyBuildingValue
			,sum(EnemyAirUnitValue) AS EnemyAirUnitValue
			,sum(ResourceValue) AS ResourceValue
		FROM resourcechange rc
		INNER JOIN regionvaluechange AS R1
			ON rc.PlayerReplayID = R1.PlayerReplayID
		WHERE rc.PlayerReplayID = PlayerA
			AND R1.Frame = (
				SELECT max(Frame)
				FROM regionvaluechange
				WHERE PlayerReplayID = PlayerA
					AND RegionID = R1.RegionID
					AND Frame <= rc.Frame
				GROUP BY RegionID
				)
		-- and rc.Frame > 0 
		GROUP BY rc.Frame
		ORDER BY rc.Frame
		) AS C
		ON AB.Frame = C.Frame
	LEFT JOIN (
		SELECT DISTINCT rc.Frame
			,sum(ResourceValue) AS ResourceValue
		FROM resourcechange rc
		INNER JOIN regionvaluechange AS R1
			ON rc.PlayerReplayID = R1.PlayerReplayID
		WHERE rc.PlayerReplayID = PlayerB
			AND R1.Frame = (
				SELECT max(Frame)
				FROM regionvaluechange
				WHERE PlayerReplayID = PlayerB
					AND RegionID = R1.RegionID
					AND Frame <= rc.Frame
				GROUP BY RegionID
				)
		-- and rc.Frame > 0 
		GROUP BY rc.Frame
		ORDER BY rc.Frame
		) AS D
		ON AB.Frame = D.Frame
	CROSS JOIN (
		SELECT @minerals := 0
			,@gas := 0
			,@supply := 0
			,@enemyMinerals := 0
			,@enemyGas := 0
			,@enemySupply := 0
			,@totMinerals := 0
			,@totGas := 0
			,@totSupply := 0
			,@winnerA := 0
			,@enemyTotMinerals := 0
			,@enemyTotGas := 0
			,@enemyTotSupply := 0
			,@winnerB := 0
			,@ground := 0
			,@building := 0
			,@air := 0
			,@enemyGround := 0
			,@enemyBuilding := 0
			,@enemyAir := 0
			,@resourcesA := 0
			,@resourcesB := 0
		) AS var_init
	ORDER BY AB.Frame;
END;
//

delete from result
select * from result
call extractReplay(266);

DROP PROCEDURE extractData;
DELIMITER //
CREATE PROCEDURE extractData ()
BEGIN

	DECLARE n_replay INT;

	DECLARE done INT DEFAULT FALSE;
    DECLARE C_Replay CURSOR FOR SELECT ReplayID FROM replay;
                                
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;
    
	OPEN C_Replay;
    
    replay_loop: LOOP
		FETCH C_Replay INTO n_replay;
        
        IF done THEN LEAVE replay_loop; END IF;
        
        CALL `sc_pvp`.`extractReplay`(n_replay);
    
    END LOOP replay_loop;
END;
//

DELETE FROM result;
call extractData;
select * from result;