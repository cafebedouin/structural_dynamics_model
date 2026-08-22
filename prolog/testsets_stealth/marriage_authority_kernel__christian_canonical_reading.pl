% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Authority over Marriage and Family (Indian Christian Marriage Act 1872)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   The Indian Christian Marriage Act 1872 codifies canonical authority over
 *   Christian marriage in India: solemnization by ordained clergy,
 *   registration that fixes status, and — through the companion Divorce Act —
 *   dissolution only on enumerated fault grounds, with church tribunals
 *   handling annulment under canon law. The arrangement coordinates genuinely
 *   (status certainty across many denominations, succession and legitimacy
 *   clarity) while extracting asymmetrically (wives carry heavier proof
 *   burdens; spouses without fault grounds cannot leave; interfaith couples
 *   are barred outright). Per the epsilon-invariance principle this file is
 *   ONE reading of the marriage_authority_kernel: the
 *   christian_canonical_reading. The four sibling readings are separate
 *   constraints with their own epsilon values, victim sets, and enforcement
 *   structures, linked through network.affects_constraints. The claim/metric
 *   gap is deliberate: claimed_type is authored from structural belief
 *   (tangled_rope — both coordination and asymmetric extraction are present),
 *   metrics from descriptive observation; the engine computes per-seat
 *   classifications independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.63).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.72).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Authority over Marriage and Family (Indian Christian Marriage Act 1872)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '8495a9b6-140a-4928-8a38-86b3ffdb521d').
narrative_ontology:cs_kernel_codification('8495a9b6-140a-4928-8a38-86b3ffdb521d', fixed_text).
narrative_ontology:cs_authority_grounding('8495a9b6-140a-4928-8a38-86b3ffdb521d', lineage).
narrative_ontology:cs_interpretation_layer_present('8495a9b6-140a-4928-8a38-86b3ffdb521d').
narrative_ontology:cs_reading_relation('8495a9b6-140a-4928-8a38-86b3ffdb521d', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('8495a9b6-140a-4928-8a38-86b3ffdb521d', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('8495a9b6-140a-4928-8a38-86b3ffdb521d', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('8495a9b6-140a-4928-8a38-86b3ffdb521d', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('8495a9b6-140a-4928-8a38-86b3ffdb521d', foundational, marriage_is_sacrament_not_mere_contract).
narrative_ontology:cs_axiom_status(marriage_is_sacrament_not_mere_contract, holdable).
narrative_ontology:cs_axiom_grounding('8495a9b6-140a-4928-8a38-86b3ffdb521d', marriage_is_sacrament_not_mere_contract, theological).
narrative_ontology:cs_axiom('8495a9b6-140a-4928-8a38-86b3ffdb521d', foundational, dissolution_confined_to_enumerated_canonical_grounds).
narrative_ontology:cs_axiom_status(dissolution_confined_to_enumerated_canonical_grounds, holdable).
narrative_ontology:cs_axiom_grounding('8495a9b6-140a-4928-8a38-86b3ffdb521d', dissolution_confined_to_enumerated_canonical_grounds, theological).
narrative_ontology:cs_reference_frame('8495a9b6-140a-4928-8a38-86b3ffdb521d', sacramental_canonical_marriage_order).
narrative_ontology:cs_drift_state('8495a9b6-140a-4928-8a38-86b3ffdb521d', contemporary_post_amendment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8495a9b6-140a-4928-8a38-86b3ffdb521d', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, ecclesiastical_hierarchies).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, ordained_clergy).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_dissolution).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, spouses_without_fault_grounds).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, interfaith_christian_couples).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, sacramental_indissolubility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Synods, bishops' conferences, and denominational councils that administer marriage tribunals, license officiants, and negotiate with the state over amendments to the governing statutes. Their canonical courts decide annulment questions for their flocks, and their positions anchor how the statutory framework operates day to day. Abandoning this role would mean surrendering the jurisdiction that constitutes their office; they operate across India.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, ecclesiastical_hierarchies, agenda_setter,
    institutional, generational, identity_locked, national).

% Priests, pastors, and ministers whose solemnization makes a marriage legally valid under the Act. The statute channels couples to them and makes their certification constitutive of marital status. They collect standing, congregational deference, and a gatekeeping role; relocating into another profession would mean abandoning vocation and community position at once.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, ordained_clergy, beneficiary,
    organized, biographical, identity_locked, regional).

% Wives who want out of marriages and must satisfy statutory fault grounds — historically proving adultery plus an aggravating ground where husbands needed adultery alone, a gap narrowed but not erased by later amendment. The proof burden falls on the petitioner, hearings run for years, a church annulment offers no civil exit, and remarriage within the congregation stays blocked until dissolution is granted.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_dissolution, payer,
    powerless, biographical, trapped, national).

% Husbands and wives in dead marriages whose circumstances match no enumerated ground — no adultery, cruelty, desertion, or conversion to plead. Mutual-consent divorce exists only where both spouses agree, so an unwilling spouse holds a veto. The realistic paths are years of separation or manufacturing a ground.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, spouses_without_fault_grounds, payer,
    moderate, biographical, trapped, national).

% Couples where one partner is not Christian cannot marry under this Act at all; its door opens only to two Christians. They are pushed to the secular civil-marriage route, sometimes under conversion pressure from families or clergy. Their objection — that the rite should be open — is heard nowhere in the framework's design.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, interfaith_christian_couples, excluded,
    moderate, biographical, mobile, national).

% District judges and family courts that hear dissolution petitions under the Divorce Act, register marriages, and decide which canonical documents receive civil recognition. They apply the statute as written; their room to reshape the arrangement is bounded by precedent and legislative text, and they collect no revenue from its operation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, civil_family_courts, agenda_setter,
    institutional, generational, constrained, national).

% The reform body that has repeatedly studied Christian personal law, circulated consultation papers on family-law reform, and recommended modernization of divorce grounds. It holds no administrative seat in the arrangement; its influence runs through reports, proposed bills, and the political process.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, law_commission_of_india, observer,
    institutional, generational, analytical, national).

% Feminist legal-aid groups and churchwomen's collectives that document the divorce burden, litigate test cases, and campaign for gender-equal grounds. They attend consultations and submit evidence but hold no vote over the statute's text or the tribunals' rules.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, women_rights_organizations, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one recognized way for Christians across many denominations in India to enter a legally valid marriage: rules on who may solemnize, registration that fixes marital status for succession and legitimacy, and a shared definition of when a marriage exists. Solves status-certainty and cross-denominational recognition problems.
% TRANSFER_FUNCTION: Moves decision-making authority over marital status from the couple to clergy and codified canonical rules; moves the cost of exiting a marriage onto the spouse seeking dissolution — disproportionately wives, who carry heavier proof burdens and longer proceedings; moves standing and gatekeeping prestige to ordained officiants and tribunals.
% ABSENT_VOICES: Spouses without fault grounds, wives carrying asymmetric proof burdens, and interfaith couples barred from the rite appear only as individual petitioners, never as participants in designing or revising the framework. Dissenting ministers who favor liberalized divorce and women's organizations hold consultation seats only, with no vote over the statute or the tribunals.
% DISAPPEARANCE_RATIONALE: Millions of registered marriages would lose their governing framework overnight; churches would lose tribunal jurisdiction and the legal constitutive force of solemnization; pending dissolution petitions would collapse into a vacuum; succession and legitimacy determinations built on the Act's registrations would unravel until a replacement regime was legislated.
% FOUNDING_PROBLEM: Colonial administration needed a single administrable marriage regime for Christian subjects: certain status, legitimate offspring, clean succession — replacing scattered denominational practice with one statute modeled on English ecclesiastical law.
% FOUNDING_PROBLEM_CORROBORATION: Churches attest a live faith-formation purpose (marriage as sacrament requiring canonical governance). Outside the benefiting parties: Law Commission consultation papers and parliamentary debate records treat the administrative-certainty problem as solved and the surviving arrangement as jurisdictional retention plus restrictive exit; high-court rulings noting that tribunal annulments lack civil effect corroborate the gap between the founding frame and current operation.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.63 at interval end: restrictive fault-based divorce, gender-asymmetric proof burdens, dual-track (canon/civil) friction, and categorical exclusion of interfaith couples, tempered by the genuine status-coordination the framework delivers. Suppression is higher (0.72) because persistence depends on actively maintained legal exit barriers plus church discipline and communal pressure — suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater is 0.36 and rising: tribunal activity is real adjudication for some questions, but a growing share consists of canonical decrees with uncertain or absent civil effect. Accessibility_collapse is 0.55: entry-time alternatives (the secular civil route) persist, but once married inside the framework the exit alternatives collapse substantially. Resistance is 0.5: sustained reform advocacy, test-case litigation, and repeated Law Commission attention, but no mass movement has dislodged the arrangement. The three measurement series share one six-point grid (1872, 1950, 1969, 2001, 2018, 2026 mapped to t=0..154) so every tracked metric is authored at every examined time point; the rising suppression_requirement series tracks the enforcement machinery hardening against reform pressure, which is the dynamic this story traces. Fixing cost is authored prohibitive: the 2001 amendment shows piecemeal adjustment is possible, but comprehensive reform (full ground parity plus subordinating tribunals to civil authority) carries communal-backlash and coalition costs that exceed what any single administrator bears from leaving the arrangement as is.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchies' seat the arrangement reads as stewardship of a sacrament they are obligated to guard; from the seats of wives seeking dissolution and spouses without fault grounds the same statutes read as a lock whose key is held by clergy and legislators. Civil courts experience neither — they apply text and collect nothing. The engine computes these divergent per-seat types from power, exit options, and declared position; the divergence between the payer seats' computed type and the beneficiary seats' computed type is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical hierarchies sit nearest the beneficiary end: they collect jurisdiction, deference, and disciplinary reach, and their identity_locked exit stabilizes their position on the subsidized side. Ordained clergy collect standing and gatekeeping prestige at low personal cost. Christian women seeking dissolution sit nearest the full-target end: powerless, with no exit short of satisfying fault grounds. Spouses without fault grounds are targets with marginally more room (mutual consent as a path, though veto-blocked). Interfaith couples are excluded rather than extracted-from; their mobile exit through the secular route dampens their pull toward the target end. Civil courts administer without collecting, sitting near symmetric. No directionality_overrides were authored: the beneficiary/victim declarations plus exit options already separate the seats, and the override surface is keyed by power atom, which would collide institutional beneficiaries with institutional administrators in this story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — colonial administrative certainty over Christian marital status — is solved; the arrangement persists serving communal identity and jurisdictional retention. Classifying it as pure extraction would erase the coordination that adherents genuinely value (sacramental continuity, cross-denominational recognition, succession clarity); classifying it as pure coordination would erase the documented asymmetric exit costs borne disproportionately by wives. The tangled_rope claim keeps both halves visible and lets the engine price each seat separately. Mandate status is contested rather than resolved: the faith-formation mandate is attested live by the churches, the administrative mandate is dead, and no arbiter outside the benefiting parties currently separates the two.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the marriage_authority_kernel — the christian_canonical_reading. Four sibling readings (hindu_codified, muslim_shariat, parsi_communal, secular_civil) instantiate different constraints from the same kernel with different victim sets, enforcement structures, and epsilon values. Where exactly does the disagreement sit?',
    'Not resolvable internally: the readings compete at the level of whose authority defines valid marriage. Resolution arrives only through constitutional adjudication or a uniform civil code enactment, which would collapse the kernel into a single reading and retire the others as live constraints.',
    'If the secular reading displaced this one, the victim set shifts from canonically-bound spouses to anyone losing communal recognition of their marriage; if a sibling communal reading hardened, this reading''s effective extraction would rise as its flock''s exit options narrowed further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one of five readings of the marriage-authority kernel; the disagreement is located in the locus of marriage-law authority.').

omega_variable(
    tribunal_civil_effect_gap,
    'Do ecclesiastical tribunal annulments carry independent civil effect, or does the dual track leave people canonically freed but civilly married (or vice versa)?',
    'Match tribunal decree records against civil registry and subsequent remarriage records; analyze high-court rulings on the recognition of canonical decrees.',
    'If tribunals'' outputs routinely lack civil effect while being treated as authoritative within the community, measured theater rises and effective suppression on the bound seats rises with it; if recognition is orderly, the dual track is coordination rather than friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_civil_effect_gap, empirical, 'Whether the church-tribunal layer performs adjudication with civil consequence or ceremonial adjudication without it.').

omega_variable(
    gender_equity_trajectory,
    'Is the residual gender asymmetry in divorce access narrowing toward parity after the 2001 amendments, or structurally stable because fault-proof burdens still fall harder on wives?',
    'Longitudinal analysis of dissolution petitions by petitioner sex, ground pleaded, success rate, and proceeding duration.',
    'Continued convergence would lower measured extraction over the next interval and shift the victim-set composition; stagnation would confirm the gendered burden as structural rather than transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_trajectory, empirical, 'Direction and rate of gender-equity change inside the arrangement.').

omega_variable(
    ucc_resolution_path,
    'Does this arrangement persist because the community prefers canonical governance, or because reform carries political costs nobody will pay?',
    'Electoral and legislative outcomes on uniform-civil-code proposals; community surveys distinguishing doctrinal attachment from status-quo inertia.',
    'If inertia dominates, the arrangement is held up by the fixing-cost asymmetry rather than preference, and persistence analysis should weight the administrator''s cost calculus heavily; if preference dominates, the coordination function is stronger than the metrics alone suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ucc_resolution_path, preference, 'Preference versus inertia as the persistence mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 0, 154).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t78, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 78, 0.18).
narrative_ontology:measurement_basis(marr_tr_t78, observed).
narrative_ontology:measurement(marr_tr_t97, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 97, 0.22).
narrative_ontology:measurement_basis(marr_tr_t97, observed).
narrative_ontology:measurement(marr_tr_t129, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 129, 0.28).
narrative_ontology:measurement_basis(marr_tr_t129, observed).
narrative_ontology:measurement(marr_tr_t146, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 146, 0.33).
narrative_ontology:measurement_basis(marr_tr_t146, observed).
narrative_ontology:measurement(marr_tr_t154, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 154, 0.36).
narrative_ontology:measurement_basis(marr_tr_t154, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t78, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 78, 0.52).
narrative_ontology:measurement_basis(marr_be_t78, observed).
narrative_ontology:measurement(marr_be_t97, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 97, 0.56).
narrative_ontology:measurement_basis(marr_be_t97, observed).
narrative_ontology:measurement(marr_be_t129, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 129, 0.57).
narrative_ontology:measurement_basis(marr_be_t129, observed).
narrative_ontology:measurement(marr_be_t146, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 146, 0.61).
narrative_ontology:measurement_basis(marr_be_t146, observed).
narrative_ontology:measurement(marr_be_t154, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 154, 0.63).
narrative_ontology:measurement_basis(marr_be_t154, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t78, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 78, 0.58).
narrative_ontology:measurement_basis(marr_su_t78, observed).
narrative_ontology:measurement(marr_su_t97, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 97, 0.6).
narrative_ontology:measurement_basis(marr_su_t97, observed).
narrative_ontology:measurement(marr_su_t129, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 129, 0.64).
narrative_ontology:measurement_basis(marr_su_t129, observed).
narrative_ontology:measurement(marr_su_t146, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 146, 0.7).
narrative_ontology:measurement_basis(marr_su_t146, observed).
narrative_ontology:measurement(marr_su_t154, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 154, 0.72).
narrative_ontology:measurement_basis(marr_su_t154, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% Constraint family: five readings of one marriage-authority kernel, held apart per the epsilon-invariance principle because 'family law authority' assessed as status-coordination yields modest extraction while the same label assessed as divorce-access yields substantial extraction — different claims, different files, different victim sets. This colonial codification sits upstream of the siblings: its restrictive-divorce template shaped later reform debates in Hindu law, and its continued operation anchors the communal-personal-law equilibrium that the secular reading contests. Each member links to the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
