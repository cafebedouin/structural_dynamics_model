% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: Decree-Sufficiency Regime of Practice Imposition (Exogenous Override Reading)
 *   domain: political_history/state_formation
 *
 * SUMMARY:
 *   A modernizing state abolishes prior social practice by decree: the old
 *   calendar is legally extinguished and official instruments re-dated;
 *   prescribed dress is mandated and proscribed garments criminalized. The
 *   doctrine in force holds that a validly enacted mandate is self-sufficient
 *   — compliance follows from legality, not from the population's conviction.
 *   The historical record the reading rests on shows a split outcome:
 *   calendar abolition produced legal-form compliance while rural life
 *   continued on inherited time until the regime itself rescinded the decree;
 *   dress edicts produced enforced public conformity through police action
 *   while private practice retreated indoors, displacing incompletely at high
 *   coercive cost. This file is ONE reading of the contested kernel
 *   legitimacy_of_imposed_practice (Rule 1 discipline): the kernel question
 *   'can law displace custom?' decomposes into three structurally distinct
 *   claims with different epsilon values — the endogenous_climb reading
 *   authors epsilon for the internalization-necessity claim, the
 *   hybrid_scaffolding reading for the messaging-reinforced claim, and this
 *   file authors epsilon for the decree-sufficiency claim over the standing
 *   decree-imposition arrangement. The claim/metric gap is deliberate: the
 *   reading CLAIMS decree sufficiency (its own doctrine) while the authored
 *   metrics describe heavily enforced, partially effective, increasingly
 *   performative imposition — the engine measures that divergence; do not
 *   reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - central_state_administration: agenda-setter and primary collector (institutional/arbitrage) — issues the decrees, staffs enforcement, captures the legibility dividends
 *   - urban_commercial_elites: beneficiary (powerful/mobile) — gain standardized dating and respectability at minimal adjustment cost
 *   - rural_peasantry: primary target (powerless/trapped) — bears re-dated obligations, disrupted rhythms, and fines without consultation
 *   - women_subject_to_dress_edicts: primary target (powerless/identity_locked) — face criminalized identity markers; respond with costly domestic withdrawal
 *   - local_clergy_and_customaries: excluded voice (organized/constrained) — custodians of the displaced practice, barred from the decree process
 *   - comparative_historians_of_state_formation: analytical observer — reconstruct formal compliance versus lived practice across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.6).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.66).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "Decree-Sufficiency Regime of Practice Imposition (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '85a72487-f728-412c-85e5-b89c08a08260').
narrative_ontology:cs_kernel_codification('85a72487-f728-412c-85e5-b89c08a08260', distributed).
narrative_ontology:cs_authority_grounding('85a72487-f728-412c-85e5-b89c08a08260', extraction).
narrative_ontology:cs_interpretation_layer_present('85a72487-f728-412c-85e5-b89c08a08260').
narrative_ontology:cs_reading_relation('85a72487-f728-412c-85e5-b89c08a08260', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('85a72487-f728-412c-85e5-b89c08a08260', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('85a72487-f728-412c-85e5-b89c08a08260', foundational, valid_decree_constitutes_binding_obligation).
narrative_ontology:cs_axiom_status(valid_decree_constitutes_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('85a72487-f728-412c-85e5-b89c08a08260', valid_decree_constitutes_binding_obligation, conventional).
narrative_ontology:cs_axiom('85a72487-f728-412c-85e5-b89c08a08260', secondary, outward_conformity_suffices_for_governance).
narrative_ontology:cs_axiom_status(outward_conformity_suffices_for_governance, holdable).
narrative_ontology:cs_axiom_grounding('85a72487-f728-412c-85e5-b89c08a08260', outward_conformity_suffices_for_governance, instrumental).
narrative_ontology:cs_reference_frame('85a72487-f728-412c-85e5-b89c08a08260', command_theory_decree_supremacy).
narrative_ontology:cs_drift_state('85a72487-f728-412c-85e5-b89c08a08260', post_case_record_synthesis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('85a72487-f728-412c-85e5-b89c08a08260', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, central_state_administration).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_commercial_elites).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_peasantry).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, women_subject_to_dress_edicts).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, decree_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, legal_positivist_command_theory).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, modernization_from_above_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and issues the decrees abolishing prior practice, staffs the enforcement chain (prefects, courts, inspectors, police), dates its own instruments in the mandated system, and collects the resulting dividends: a population that is taxable on schedule, conscriptable on schedule, and legible from the center. It can amend or rescind the arrangement by the same authority that created it; its main cost in doing so is admitting doctrinal failure.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, central_state_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, central_state_administration, beneficiary).

% Merchants, creditors, lawyers, and officials in cities where the mandated practice matched habits they already held or could adopt cheaply. Uniform contract dating lowers their transaction costs; visible conformity signals respectability and access to office. They bear almost none of the adjustment burden and can relocate their affairs if enforcement turns disruptive.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_commercial_elites, beneficiary,
    powerful, biographical, mobile, national).

% Work land, run livestock, and attend fairs on rhythms inherited across generations. The decree re-dates their leases, taxes, and market days without asking them, severing commerce from harvest and liturgy. Compliance means retooling bookkeeping and forfeiting festival economies; non-compliance means fines and missed markets. Tied to land and parish, they cannot leave the jurisdiction, so they comply on paper and persist in practice — signing the mandated forms while living by the old count.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_peasantry, payer,
    powerless, biographical, trapped, regional).

% Wear garments that mark religious and communal belonging; the edict criminalizes those garments in public and backs the ban with police, fines, and public humiliation. Adopting the prescribed dress means shedding identity markers that constitute their standing in family and congregation; refusing means confinement — many withdraw from streets, markets, and employment altogether. Their characteristic response is a costly retreat that shrinks their own lives rather than either compliance or escape.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, women_subject_to_dress_edicts, payer,
    powerless, biographical, identity_locked, national).

% Custodians of the displaced practice — the liturgical calendar that structured rural time, the dress norms that marked community membership. They lose authority and standing when the state transfers those functions to itself, and they object through sermons, petitions, and communal leadership. They are barred from the decree process entirely, and when they become focal points of non-compliance they invite surveillance and repression, narrowing what objection can safely say.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, local_clergy_and_customaries, excluded,
    organized, generational, constrained, regional).

% Reconstruct the record across the calendar-abolition and dress-campaign cases, cross-checking administrative archives against parish registers, household accounts, and private correspondence to separate formal compliance from lived practice. Their findings are the principal external check on the doctrine's self-assessment, and their publications shape whether later states attempt similar impositions.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, comparative_historians_of_state_formation, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Imposes uniform public standards across a heterogeneous territory: one legal calendar for contracts, taxation, courts, and administration; one visible dress norm marking national membership. This solves the coordination problem that fragmented local calendars and dress codes posed for a centralizing state and for interregional exchange.
% TRANSFER_FUNCTION: Moves decision authority over daily practice from households, parishes, and guilds to the central state; moves adjustment costs — retooled accounting, disrupted festivals and market days, abandoned garments, withdrawn public life — onto rural populations and identity-marked women without compensation; moves prestige and demonstrated sovereignty to the modernizing state and its enforcement personnel.
% ABSENT_VOICES: The rural populations and the women subject to the dress edicts were never consulted — decree is precisely the mode of rule that excludes consent, and their objection would have been decisive against the adjustment-cost side of the ledger. Local clergy and customary officeholders who did object were barred from the drafting process and, when they anchored non-compliance, repressed. Their dissent survives only in petitions, sermons, and archive fragments assembled retrospectively by historians.
% DISAPPEARANCE_RATIONALE: If decree authority over practice vanished overnight, contract dating, tax schedules, market-day regulation, and public-order arrangements keyed to the mandated standards would unravel until replaced by negotiated local equivalents or fresh decrees; enforcement careers built on the edicts would dissolve; and the state's claim to have modernized by mandate would stand visibly hollow, forcing either retraction or escalation.
% FOUNDING_PROBLEM: Post-revolutionary and post-imperial states inherited territorially heterogeneous societies — dozens of local calendars, dialects, and dress codes — that obstructed taxation, conscription, courts, and national integration. The founding problem was legibility: making diverse populations administrable from a center.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the comparative state-formation literature and the administrative archives themselves attest that the fragmentation problem was real — while the same external sources document that decree-only displacement largely failed and that durable standardization came through schooling, conscription, and market integration over generations. Modernizing officials and their heirs attest the problem as live and the decree remedy as vindicated; historians of the period attest it as real but substantially resolved by slower means. No attesting source inside the beneficiary set is accepted here without the external counterpart.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.60 at interval end) because adjustment costs — retooled accounting, disrupted festivals and market days, abandoned garments, withdrawn public life — fall entirely on populations who were never consulted and receive no compensating benefit; it peaks mid-interval when enforcement intensity and adjustment costs stack, then recedes as the calendar case terminates by rescission and dress enforcement settles into selective policing. Suppression (0.66) is high because persistence depends on continuous coercive input: the old practice remains fully available in private life, so the constraint must patrol the boundary between public form and private habit indefinitely. Accessibility collapse is LOW (0.38) — this is the decisive structural fact against any mountain-like reading: the displaced practice does not become unthinkable or unavailable; it survives intact one threshold away (the home, the village, the private ledger), which is why enforcement can never stand down. Resistance is correspondingly high (0.62): quiet persistence, market-day stubbornness, domestic withdrawal. Theater ratio rises monotonically to 0.50 — an increasing share of observable 'compliance' is nominal performance (documents signed in mandated dates while ledgers run on inherited time; prescribed dress worn in town, shed at the door), the classic signature of a mandate outrunning internalization. All three series are authored on one shared seven-point grid (t=0..36, step 6) so no metric row borrows another's endpoint. Gain flow: the administration captures the constraint's gains (taxable, schedulable, legible population), so gain_flow names that seat rather than 'diffuse'. Fixing cost is 'cheap': the same decree authority that imposed can rescind — the calendar was abolished by a stroke of the pen — so the barrier to removal is doctrinal prestige and enforcement careers, not structural lock-in; that cheapness is itself evidence the mandate, not necessity, is doing the holding.
 *
 * PERSPECTIVAL GAP:
 *   Three seats compute differently from identical structure. From the agenda-setter seat the arrangement is its own demonstrated sovereignty: contracts arrive dated correctly, the streets look uniform, and each year of quiet filing confirms the doctrine — decree works. From the payer seats the same years are uncompensated coercion: fines for dressing as one's grandmothers dressed, market calendars severed from harvest and liturgy, and a choice between identity and public life. From the urban beneficiary seat it is mild convenience. The engine computes these divergent classifications from the structural data (power, exit, directionality); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Central state administration sits at the beneficiary pole (d near 0.05): it writes the rule, collects the legibility dividend, and can exit by rescinding — arbitrage-grade exit damps its exposure to near zero. Urban commercial elites sit near-beneficiary (d near 0.15): they collect standardization gains and their prior practice was already adjacent to the mandate, so adjustment cost approximates zero. Rural peasantry sit near the target pole (d near 0.85): full adjustment cost, no consultation, trapped exit amplifying effective extraction. Women subject to dress edicts sit nearest the target pole (d near 0.95): identity-locked exit means the only exits are abandonment of religious-communal identity or withdrawal from public life, both of which are costs rather than escapes. National spatial scope modestly amplifies effective extraction on the payer seats (verification across villages is hard; enforcement is uneven but inescapable in public). Excluded clergy feed no directionality — they are outside the arrangement's bargain, which is precisely the complaint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — territorial illegibility obstructing taxation, conscription, courts, and integration — was real, and the constraint carries a genuine coordination dividend: uniform dating measurably lowered transaction costs, and a single public standard did aid administration. That dividend blocks a pure-extraction reading. But the victims are equally real and uncompensated, which blocks a pure-coordination reading; hence the tangled_rope claim. On mandatrophy: the mandate (displace backwardness by decree) has substantially outlived its function — durable standardization arrived through schooling, conscription, and markets over generations, not through the decrees, and the decree regimes ended by rescission or decay rather than by declared completion. What persists in the record's later stretch is largely prestige performance and enforcement careers maintained around a partially hollowed mandate. The classification prevents mislabeling in both directions: reading the theater ratio alone would call this a dead mandate kept alive for show (piton-flavored); reading the enforcement machinery alone would call it pure predation (snare-flavored); the structural data — real coordination function, identifiable payers, mandatory enforcement — holds it as a tangled rope trending toward theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the exogenous_override_reading of the kernel legitimacy_of_imposed_practice; what would the sibling readings (endogenous_climb_reading, hybrid_scaffolding_reading) change structurally?',
    'Compile and compare the full reading set of the kernel; locate the disagreement in the sufficiency-versus-necessity premise about what makes imposed practice stick.',
    'The endogenous_climb reading would shift emphasis to failed-adoption losses and discount decree-regime achievements as illusory; the hybrid reading would partition the measured effects between mandate machinery and ideological pull, changing both epsilon attribution and the victim set''s composition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame placement: one reading of a three-reading kernel, not the topic whole.').

omega_variable(
    compliance_displacement_gap,
    'Does the historical record distinguish formal compliance (contracts dated in the mandated system, garments worn in public view) from actual displacement of lived practice (household timekeeping, market rhythms, private dress)?',
    'Longitudinal comparison of administrative archives against household accounts, parish registers, market-day records, and private correspondence across the calendar-abolition and dress-edict cases.',
    'If the gap is wide, the reading''s sufficiency claim reduces to coerced performance: effective extraction concentrates on payer seats, the theater ratio understates nominalism, and the constraint shifts snare-ward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_displacement_gap, empirical, 'Whether the decree produced practice change or only observable conformity.').

omega_variable(
    suppression_structural_vs_habituated,
    'Is the observed conformity maintained by active enforcement (structural suppression) or by emerging habituation that would survive enforcement relaxation?',
    'Post-relaxation trajectory: track whether prior practice resurges when enforcement capacity drops or regimes change; resurgence indicates the suppression was structural, persistence indicates partial internalization.',
    'If habituation dominates, the suppression measure is overstated and the constraint trends coordination-ward; if structural, persistence depends on continuous coercive input and the enforcement series is load-bearing for classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_habituated, empirical, 'Structural versus habituated mechanism behind observed conformity.').

omega_variable(
    enforcement_severity_confound,
    'Where decree regimes appear to succeed, is the success attributable to decree sufficiency as such, or to enforcement severity plus generational turnover doing the displacement work?',
    'Cross-case comparison holding decree content constant while varying enforcement intensity and cohort replacement rates; isolate cases where identical mandates met different enforcement budgets.',
    'Attribution to severity and cohort turnover undermines the reading''s foundational axiom, supports the hybrid reading''s scaffolding account, and implies the measured compliance was purchased, not commanded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_severity_confound, empirical, 'Confound between decree authority and the enforcement/cohort machinery accompanying it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(loip_exo_override_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(loip_exo_override_tr_t6, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(loip_exo_override_tr_t12, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(loip_exo_override_tr_t18, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 18, 0.39).
narrative_ontology:measurement(loip_exo_override_tr_t24, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(loip_exo_override_tr_t30, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement(loip_exo_override_tr_t36, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 36, 0.5).

% Extraction over time
narrative_ontology:measurement(loip_exo_override_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(loip_exo_override_be_t6, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement(loip_exo_override_be_t12, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(loip_exo_override_be_t18, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(loip_exo_override_be_t24, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(loip_exo_override_be_t30, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(loip_exo_override_be_t36, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 36, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(loip_exo_override_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(loip_exo_override_su_t6, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement(loip_exo_override_su_t12, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(loip_exo_override_su_t18, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(loip_exo_override_su_t24, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(loip_exo_override_su_t30, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(loip_exo_override_su_t36, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 36, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'can law displace custom?' decomposes per the epsilon-invariance principle into three structurally distinct claims, each with its own epsilon, victims, and classification — endogenous_climb_reading (internalization necessary; epsilon authored over the adoption-failure arrangement), exogenous_override_reading (this file; decree sufficient; epsilon authored over the standing decree-imposition arrangement), hybrid_scaffolding_reading (mandate-plus-messaging; epsilon authored over the scaffolded imposition arrangement). Edges: this reading is upstream of the hybrid reading (hybrid presupposes functioning mandate machinery) and linked to the climb reading as its principal rival. The upstream story (higher-confidence enforcement record) influences the downstream contested story, mirroring the BGS family pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
