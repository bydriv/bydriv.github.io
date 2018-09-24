export function enemy(team1, team2) {
    switch (team1) {
    case "player":
        switch (team2) {
        case "player":
            return false;
        case "enemy":
            return true;
        }
    case "enemy":
        switch (team2) {
        case "player":
            return true;
        case "enemy":
            return false;
        }
    }
}
