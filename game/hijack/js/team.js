export function enemy(object1, object2) {
    if (object1.rejectedIds.some(id => id === object2.id))
        return true;
    if (object1.acceptedIds.some(id => id === object2.id))
        return false;

    const team1 = object1.team;
    const team2 = object2.team;

    switch (team1) {
    case "player":
        switch (team2) {
        case "player":
            return false;
        case "enemy":
            return true;
        case "neutral":
            return false;
        }
    case "enemy":
        switch (team2) {
        case "player":
            return true;
        case "enemy":
            return false;
        case "neutral":
            return false;
        }
    case "neutral":
        return false;
    }
}

export function color(team) {
    switch (team) {
    case "player":
        return {
            fg: 0x00FF00,
            bg: 0x008000
        };
    case "enemy":
        return {
            fg: 0xFF0000,
            bg: 0x800000
        };
    }
}
