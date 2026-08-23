% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Scaffolded Imposition Apparatus for Practice Displacement (Hybrid Scaffolding Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A consolidating state mandates a new national practice — a standard
 *   calendar, a prescribed dress — and does not rely on the decree alone: it
 *   builds a scaffolding of elite modeling and ideological messaging intended
 *   to make adoption feel self-generated. State schools, an urban press, and
 *   appointed notables display the practice in dignified use, and the
 *   messaging frames adoption as national modernity rather than submission to
 *   decree. On the record this reading organizes, the arrangement achieves
 *   partial displacement: hybrid practices spread where the scaffolding
 *   reaches, customary practice persists privately and in rural regions
 *   beyond its reach, and displacement stalls at hybridity rather than
 *   completing. The gains accrue to urban elites whose Western markers the
 *   scaffolding subsidizes and renders prestigious, and to the state's
 *   treasury and administrative legibility; the costs land on rural
 *   populations who are subject to the mandate but excluded from the schools,
 *   press, and networks that make compliance advantageous. This file
 *   instantiates the hybrid_scaffolding_reading only, as a clean
 *   single-epsilon constraint; the sibling readings of the same kernel are
 *   separate constraints (see kernel_context). The claimed type and the
 *   metrics are authored independently: the claim is tangled_rope, and the
 *   metrics describe the arrangement as it actually operates.
 *
 * KEY AGENTS:
 *   - central_state_administration: agenda-setter (institutional/arbitrage) — issues the mandate, funds the messaging apparatus and inspectorate, collects fines, legibility, and international standing
 *   - westernized_urban_elites: primary beneficiary (powerful/constrained) — model the practice, collect status and appointments, bear modeling and adoption costs
 *   - urban_middle_strata: partial beneficiary (moderate/constrained) — aspirational adopters with partial infrastructure access, hybrid practice, adoption costs without the full payoff
 *   - rural_peasantries: primary target (powerless/trapped) — subject to the mandate, outside the scaffolding, pay fines and rework daily practice
 *   - customary_practice_leaders: secondary target and excluded voice (organized/identity_locked) — authority fused with the old practice, counter-framing barred from official channels
 *   - comparative_historical_analysts: analytical observer — hold the cross-case reversion and hybridity record the campaigns' own reports omit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.62).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Scaffolded Imposition Apparatus for Practice Displacement (Hybrid Scaffolding Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '248b56f8-73c4-4f17-be40-9563f71ff6c3').
narrative_ontology:cs_kernel_codification('248b56f8-73c4-4f17-be40-9563f71ff6c3', distributed).
narrative_ontology:cs_authority_grounding('248b56f8-73c4-4f17-be40-9563f71ff6c3', distributed).
narrative_ontology:cs_reading_relation('248b56f8-73c4-4f17-be40-9563f71ff6c3', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('248b56f8-73c4-4f17-be40-9563f71ff6c3', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom('248b56f8-73c4-4f17-be40-9563f71ff6c3', foundational, bare_decree_insufficient_for_displacement).
narrative_ontology:cs_axiom_status(bare_decree_insufficient_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('248b56f8-73c4-4f17-be40-9563f71ff6c3', bare_decree_insufficient_for_displacement, empirically_contingent).
narrative_ontology:cs_axiom('248b56f8-73c4-4f17-be40-9563f71ff6c3', foundational, messaging_reinforced_mandates_generate_quasi_endogenous_pull).
narrative_ontology:cs_axiom_status(messaging_reinforced_mandates_generate_quasi_endogenous_pull, holdable).
narrative_ontology:cs_axiom_grounding('248b56f8-73c4-4f17-be40-9563f71ff6c3', messaging_reinforced_mandates_generate_quasi_endogenous_pull, empirically_contingent).
narrative_ontology:cs_axiom('248b56f8-73c4-4f17-be40-9563f71ff6c3', secondary, scaffolding_dominates_both_pure_paths).
narrative_ontology:cs_axiom_status(scaffolding_dominates_both_pure_paths, holdable).
narrative_ontology:cs_axiom_grounding('248b56f8-73c4-4f17-be40-9563f71ff6c3', scaffolding_dominates_both_pure_paths, instrumental).
narrative_ontology:cs_reference_frame('248b56f8-73c4-4f17-be40-9563f71ff6c3', scaffolded_transition_baseline).
narrative_ontology:cs_drift_state('248b56f8-73c4-4f17-be40-9563f71ff6c3', contemporary_reversion_studies, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('248b56f8-73c4-4f17-be40-9563f71ff6c3', '2026-08-10T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, westernized_urban_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, central_state_administration).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_peasantries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_middle_strata).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, westernized_urban_elites).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, customary_practice_leaders).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, hybrid_scaffolding_thesis).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, quasi_endogenous_pull_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the mandate making the new calendar and dress compulsory, funds the schools, newspapers, and inspectorate that carry the campaign, and appoints the notables who model the practice in public. Collects fines from noncompliance, gains administrative uniformity and international standing as the practice spreads, and can redirect or retire the campaign at will, answering to no outside authority that could impose a practice on it.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, central_state_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Staff the campaign's ministries, schools, and press; model the new dress and calendar in dignified public use; and collect the status that Western markers carry in the new order, including appointments, urban standing, and marriage and business networks. They also bear the cost of performing the model role and of adopting practices that sit awkwardly with their own family customs, and their standing depends on the campaign's continuation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, westernized_urban_elites, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, westernized_urban_elites, payer).

% Aspire to elite standing and gain partial access to the campaign infrastructure — state schools, urban employment, the ideological press — that makes adoption affordable. They adopt selectively, keeping hybrid wardrobes and dual calendars, and pay adoption costs without the full elite payoff; falling out of the urban economy would strip them of the access that makes compliance worthwhile.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_middle_strata, beneficiary,
    moderate, biographical, constrained, national).

% Are subject to the mandate but outside the campaign's reach: no state schools nearby, no ideological press in their idiom, no elite networks to join. Inspectors and gendarmerie arrive to enforce what the cities model. They pay fines, rework daily life around a calendar and dress that serve distant administrators, and keep customary practice alive privately where enforcement is thin; leaving the jurisdiction is not a real option and their villages hold no seat in the campaign's design.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_peasantries, payer,
    powerless, biographical, trapped, regional).

% Clerics, elders, and guild heads whose authority rests on the old calendar and dress. They organize quiet noncompliance and seasonal reversions, and their counter-framing of the campaign is barred from the official press. Their standing is fused with the practice under displacement — abandoning it would dissolve the authority they hold — so they defend it even where open defense carries fines and surveillance.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, customary_practice_leaders, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, customary_practice_leaders, excluded).

% Compare imposition campaigns across states and decades, tracking which practices displaced, which hybridized, and which reverted once enforcement lifted. They hold no stake in any campaign's outcome and publish the reversion and hybridity records that the campaigns' own success reports omit.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, comparative_historical_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, central_state_administration).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the transition-synchronization problem: displacing a heterogeneous customary practice with a single national practice requires that adopters move on a visible, prestigious signal rather than each waiting for the other. Elite modeling shows what the new practice looks like in dignified use, and the ideological framing tells adopters why the move raises rather than lowers their standing, collapsing the first-mover social risk that a bare decree leaves unsolved.
% TRANSFER_FUNCTION: Moves compliance and fines from rural populations to the state treasury; moves cultural authority from customary leaders to state-aligned urban elites; moves status and identity capital toward whoever holds access to the campaign's schools, press, and networks, priced in the abandonment of prior practice.
% ABSENT_VOICES: Rural communities were the campaign's objects, never its authors: no village seat sat in the ministry rooms where the mandate and its messaging were drafted, and customary authorities' counter-arguments were barred from the official press. Seated at the table, they would ask why the practice serves urban self-presentation at rural expense, and would demand either exemption or the same schools, press, and networks the cities received.
% DISAPPEARANCE_RATIONALE: Overnight removal would freeze displacement mid-transition: hybrid practices would revert toward customary forms where enforcement memory faded, urban elites would lose the state backing that makes their markers prestigious, the inspectorate and press apparatus would dissolve into ordinary administration, and the state would forfeit the uniformity and international standing the campaign was buying. Every named party's position depends on the arrangement's continuation.
% FOUNDING_PROBLEM: A consolidating state needed one legible national practice — a single calendar, a recognizable national dress — to standardize administration and present itself as a modern peer to other states, within a state-formation horizon that neither waiting for bottom-up adoption nor issuing bare decrees (which had demonstrably failed in the calendar case) could meet.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historical scholarship, from outside the benefiting parties, attests both halves: the founding problem was real (consolidating states did need practice standardization) and the scaffolded route did outperform bare decree. Rural-history and anthropological records, likewise outside the beneficiary set, attest that the problem as framed was the center's problem, that displacement stalled at hybridity, and that the costs landed on populations never consulted. No party inside the arrangement attests the founding problem neutrally.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 because the arrangement transfers real goods — fines, labor-time reworked around an alien calendar, the dismantled authority of customary leaders — from populations that receive none of the scaffolding, while the coordination it provides is genuine but priced regressively. Suppression at 0.58 reflects an inspectorate, fines, and gendarmerie enforcement of dress and calendar compliance, tempered by the fact that much compliance is pulled rather than pushed once the messaging takes hold. Theater at 0.32 reflects a campaign whose success reporting outruns its displacement: staged adoption ceremonies, inflated compliance statistics, and model-family displays grow as real displacement plateaus at hybridity. Accessibility collapse is 0.45 — alternatives persist in private and rural practice, and hybrid forms remain a live middle option, so understanding the mandate does not close the exit to customary practice. Resistance at 0.50 reflects sustained quiet noncompliance, seasonal reversion, and organized customary-leader opposition. The measurement series share one time grid (t = 0, 5, 10, 15, 20, 25): extractiveness climbs as the campaign extends into new practice domains and the urban/rural differential locks in; theater climbs with the success-reporting apparatus; suppression_requirement is non-monotonic — an enforcement ratchet through the build-out years (t=0-15) as the inspectorate matures, then a managed relaxation (t=15-25) as quasi-endogenous pull reduces the coercion needed to hold measured compliance. End-state values equal the base_properties values.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same arrangement. From the agenda-setter and elite seats, the scaffolding is nation-building coordination they built and staff: the mandate is the price of modernity, the messaging is education, and the hybrid outcome is a success. From the rural payer seat, the same structure is a fine-collecting, practice-breaking apparatus that subsidizes urban self-presentation at rural expense — the decree arrives with inspectors, but the schools and press that make compliance advantageous never arrive. Customary leaders experience a third structure: an assault on the authority fused with their office. The engine computes these per-seat classifications from power, exit, and directionality; the divergence between the elite seat's coordination experience and the rural seat's extraction experience is the arrangement's defining asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The state administration sits near the beneficiary pole: agenda-setter collecting fines, legibility, and standing, with arbitrage-grade control over which practice to mandate next. Westernized urban elites sit near the beneficiary pole as well — subsidized adoption, prestige returns — damped slightly by their modeling and adoption costs. Urban middle strata sit mid-range: real benefits from partial access, real costs of adoption without the full elite payoff. Rural peasantries sit at the target pole: full mandate exposure, zero scaffolding access, trapped exit — the arrangement's extraction concentrates on them at national scope, which amplifies effective extraction through the scope-verification channel. Customary practice leaders sit at the target pole with identity lock: their office IS the old practice, so exit would dissolve the authority they hold, raising their effective extraction and explaining their organized resistance. Rural peasantries retain coalition potential — village networks and customary-leader-led noncompliance — but it is fragmented by the same infrastructure differential that excludes them, so their effective power stays near powerless.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents mislabeling in both directions. Reading the arrangement as pure extraction would erase the real coordination it performs — transition synchronization that bare decree could not deliver — and would wrongly predict that removing it returns the world to a prior equilibrium rather than freezing displacement mid-transition. Reading it as pure coordination would erase the rural payer seat whose costs fund urban gains and whose exclusion is not an accident of budgeting. The mandatrophy question — has the mandate outlived its function? — turns on the founding problem: displacement stalled at hybridity, so the arrangement's transitional justification is half-spent while the apparatus persists and grows a success-reporting layer (theater rising from 0.14 to 0.32 across the interval). If the founding problem were declared dead while the world still rearranges around the apparatus, the mismatch consumer would flag a zombie; this story authors the status as contested, which is the honest state — the standardization half is achieved, the displacement half is not, and the parties dispute whether the remainder justifies the apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (hybrid_scaffolding_reading) of the kernel legitimacy_of_imposed_practice; would the exogenous_override_reading or endogenous_climb_reading of the same kernel instantiate a different constraint with a different victim set and epsilon?',
    'Author the sibling readings as separate stories over the same evidentiary base (the calendar case run by bare decree; the dress case run with elite modeling and ideological framing) and compare computed classifications across the family.',
    'The exogenous_override reading would attribute the calendar''s failure to enforcement shortfall rather than decree insufficiency, collapsing the victim structure into mere noncompliance; the endogenous_climb reading would read the scaffolding as a futile overlay on a bottom-up process, reattributing elite gains to selection rather than scaffolding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    calendar_case_generality,
    'Is the bare-decree failure that anchors this reading''s contrast domain-general, or an artifact of the calendar specifically — a practice with no prestige economy, no elite payoff, and no wearable display surface?',
    'Survey imposition campaigns across practice domains, coding for the presence of an elite status payoff; test whether decree-only success tracks payoff presence rather than enforcement intensity.',
    'If the calendar failed for idiosyncratic reasons, the hybrid reading''s mechanism claim narrows to display practices with status economies, and the arrangement''s coordination claim weakens accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calendar_case_generality, empirical, 'Whether the decree-failure contrast generalizes beyond the calendar case.').

omega_variable(
    quasi_endogenous_pull_depth,
    'Is the pull the messaging generates genuine internalization, or measured adoption that reverts when enforcement relaxes — is quasi-endogenous pull internalization, or compliance wearing internalization''s clothes?',
    'Track practice retention after enforcement withdrawal across scaffolded campaigns; distinguish retained hybrid forms from reversion, and interview adopters on private practice.',
    'If adoption is enforcement-contingent, the scaffolding''s coordination function is thinner than claimed and its operation shifts toward enforced compliance with theatrical internalization — raising effective suppression and theater and pressing per-seat classifications toward the extractive pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quasi_endogenous_pull_depth, empirical, 'Depth of the quasi-endogenous pull: internalization versus enforcement-contingent compliance.').

omega_variable(
    rural_exclusion_functionality,
    'Is rural exclusion from the scaffolding an incidental budget constraint, or functional to the arrangement — does the elite status premium depend on the practice remaining scarce and urban?',
    'Compare with campaigns that extended scaffolding to rural areas at fiscal cost: did elite adoption and status premiums persist, and did displacement deepen or stall?',
    'If exclusion is functional, the asymmetry is constitutive rather than incidental — the arrangement cannot be fixed by extending access without dissolving the elite payoff that powers it, and the extraction reading hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rural_exclusion_functionality, conceptual, 'Whether rural exclusion is incidental or constitutive of the elite payoff.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imposed_practice_hybrid_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(imposed_practice_hybrid_tr_t5, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(imposed_practice_hybrid_tr_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(imposed_practice_hybrid_tr_t15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(imposed_practice_hybrid_tr_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(imposed_practice_hybrid_tr_t25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 25, 0.32).

% Extraction over time
narrative_ontology:measurement(imposed_practice_hybrid_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(imposed_practice_hybrid_be_t5, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(imposed_practice_hybrid_be_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(imposed_practice_hybrid_be_t15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(imposed_practice_hybrid_be_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(imposed_practice_hybrid_be_t25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(imposed_practice_hybrid_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(imposed_practice_hybrid_su_t5, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(imposed_practice_hybrid_su_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(imposed_practice_hybrid_su_t15, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(imposed_practice_hybrid_su_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(imposed_practice_hybrid_su_t25, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'legitimacy of imposed practice' decomposes into three readings of one contested kernel; this file is the hybrid_scaffolding_reading. The calendar case (bare decree, failed) and the dress case (scaffolded, partial success with hybrid practices) are the shared evidentiary base that the readings weigh differently; the sibling files carry their own epsilon, beneficiary/victim structure, and classification. Links here are family links for contamination analysis, not causal edges. A further per-case decomposition (separate calendar-mandate and dress-mandate stories) is available if epsilon proves unstable across practice domains; this story authors the mechanism-level arrangement per the reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
