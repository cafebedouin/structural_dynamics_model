% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__beneficiary_agency_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Coalition as Strategic Authority-Bypass Vehicle
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the beneficiary-agency reading of the
 *   technology_reformation_causality kernel: reformist clergy and
 *   commercially motivated printers strategically deployed print to bypass
 *   Church authority over scriptural interpretation, with the press
 *   functioning as a scaffold — a tool selected and wielded — rather than as
 *   an independent causal force. The coalition between reformers (who
 *   supplied content and legitimacy) and printers (who supplied capital,
 *   distribution, and jurisdictional mobility) constitutes a tangled_rope: it
 *   coordinates genuine mass distribution of vernacular theology while
 *   extracting authority from the Church hierarchy and imposing costs on
 *   illiterate peasants and unaligned printers who had no say in the
 *   arrangement. ε is derived here specifically from the VALUE of the
 *   authority bypassed (control over doctrinal interpretation,
 *   tithe/indulgence revenue streams, and confessional uniformity), not from
 *   any property of movable type itself.
 *
 * KEY AGENTS:
 *   - reformist_clergy: primary agenda-setter and beneficiary, deliberately commissions and times print output
 *   - printer_guild_operators: co-beneficiary, supplies capital and mobility, profits from patronage
 *   - roman_church_hierarchy: primary target, loses interpretive monopoly and enforcement capacity
 *   - illiterate_peasantry: diffuse victim, bears confessional-conflict costs without benefiting from vernacular access
 *   - rival_unlicensed_printers: excluded competitor, loses market access to the patronage-protected coalition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.68).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.55).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition as Strategic Authority-Bypass Vehicle").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, '7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1').
narrative_ontology:cs_kernel_codification('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', distributed).
narrative_ontology:cs_authority_grounding('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', distributed).
narrative_ontology:cs_reading_relation('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', technology_reformation_causality__co_constitution_reading, influences).
narrative_ontology:cs_axiom('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', foundational, agency_precedes_and_directs_technological_effect).
narrative_ontology:cs_axiom_status(agency_precedes_and_directs_technological_effect, holdable).
narrative_ontology:cs_axiom_grounding('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', agency_precedes_and_directs_technological_effect, empirically_contingent).
narrative_ontology:cs_axiom('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', secondary, technology_as_selected_instrument_not_independent_cause).
narrative_ontology:cs_axiom_status(technology_as_selected_instrument_not_independent_cause, holdable).
narrative_ontology:cs_axiom_grounding('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', technology_as_selected_instrument_not_independent_cause, conventional).
narrative_ontology:cs_reference_frame('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', papal_scriptural_interpretive_monopoly).
narrative_ontology:cs_drift_state('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', post_reformation_consolidation, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('7a39fad5-ffdd-4bc0-ae14-5b751f90f0d1', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, reformist_clergy).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, printer_guild_operators).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, vernacular_literate_burghers).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, roman_church_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, illiterate_peasantry).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, rival_unlicensed_printers).
narrative_ontology:constraint_vindicates(technology_reformation_causality__beneficiary_agency_reading, agency_over_determinism_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deliberately commissions vernacular translations and polemical pamphlets, selects printers, times releases to coincide with disputes, and uses print runs to outpace episcopal condemnation. Builds a durable alternative authority structure funded and legitimated by the same print output it directs.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, reformist_clergy, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, reformist_clergy, beneficiary).

% Print runs of theological tracts and vernacular bibles are commercially lucrative and politically protected once aligned with a reforming prince or magistrate. Printers choose which texts to run based on profit and patronage, relocating operations to friendlier jurisdictions when threatened.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, printer_guild_operators, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, printer_guild_operators, agenda_setter).

% Urban literate laity gain direct access to scripture and doctrinal argument without clerical mediation, and gain standing in new congregational structures built around printed materials they can read and circulate themselves.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, vernacular_literate_burghers, beneficiary,
    moderate, biographical, constrained, regional).

% Loses monopoly control over scriptural interpretation and doctrinal transmission as printed vernacular texts circulate faster than censorship or excommunication can suppress them. Attempts indices of prohibited books and licensing regimes that consistently lag print volume.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, roman_church_hierarchy, payer,
    institutional, civilizational, constrained, continental).

% Excluded from the direct benefit of vernacular print (cannot read it) while bearing the social and sometimes violent costs of confessional conflict — peasant wars, forced conversions, communal splits — driven by literate elites' theological disputes conducted through print.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, illiterate_peasantry, payer,
    powerless, biographical, trapped, local).

% Printers who lack access to reformist patronage networks or magisterial protection are prosecuted, have presses seized, or are undercut by subsidized reformist print runs; the coalition's success forecloses their market access.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, rival_unlicensed_printers, payer,
    moderate, biographical, constrained, regional).

% Debate whether print technology or reformer agency was the operative cause of the Reformation's spread; this reading credits deliberate strategic deployment by reformers and printers rather than the technology's inherent properties.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, confessional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, reformist_clergy).
narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reformers and printers jointly solve a genuine coordination problem: rapid, wide, low-cost distribution of theological argument that lets a fragmented reform movement act with continental reach faster than a centralized hierarchy can respond.
% TRANSFER_FUNCTION: Moves interpretive and doctrinal authority from the Church hierarchy to reformist clergy and literate laity, and moves commercial profit and political protection to printers aligned with reform patrons — at the cost of confessional stability borne by peasant communities and market access borne by unaligned printers.
% ABSENT_VOICES: Illiterate peasants who bear the costs of confessional violence have no voice in the print-mediated disputes; rival printers excluded from patronage networks are prosecuted rather than heard; the Church's own theological objections are voiced but are structurally out-paced rather than out-argued.
% DISAPPEARANCE_RATIONALE: This reading holds the coalition's strategic choices, not the press itself, as decisive — if THIS coalition (this set of reformers and this patronage network) had not formed, other agents might have deployed the same technology toward different or no reformist ends; the sibling determinist reading holds the opposite. The disagreement is genuinely contested within the historiography, not resolved by this story.
% FOUNDING_PROBLEM: Reformist clergy needed a distribution mechanism that could outpace ecclesiastical censorship and reach lay audiences directly, bypassing the Church's control of scriptural interpretation and liturgical language.
% FOUNDING_PROBLEM_CORROBORATION: Church polemicists of the period (e.g. contemporary Catholic controversialists cataloguing prohibited books) attest from outside the reformist coalition that the bypass function was real and effective; modern book-history scholarship (Eisenstein's critics and defenders alike) independently corroborates that the original censorship-evasion problem was resolved within decades, after which print's role shifted to confessional consolidation rather than authority bypass.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 at interval end) is authored high because the coalition's success measurably transfers a specific, valuable asset — doctrinal interpretive authority and its associated revenue (indulgences, tithes enforced through excommunication threat) — from the Church to reformist institutions and their printer allies. Suppression (0.55) reflects the coalition's active use of patronage-protected jurisdictions, licensing arrangements with sympathetic magistrates, and market exclusion of unaligned printers — this is coercive infrastructure, not mere market competition. Theater ratio rises over the interval (0.15 to 0.40) as the original bypass function is substantially achieved by mid-century and later print activity increasingly performs confessional identity-maintenance rather than continuing active authority evasion — this is the founding-problem-status=dead signal made visible temporally.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist clergy and printers are declared beneficiaries with mobile exit (they relocate operations and switch patrons when threatened) — this derives low-to-symmetric directionality despite organized power, because the derivation correctly reads their agency as the operative structural fact of this reading. The Church hierarchy is a payer with constrained exit despite institutional power — it cannot simply relocate its claim to interpretive authority once print has enabled durable alternative distribution channels; its high time_horizon (civilizational) amplifies the effective loss. Illiterate peasantry sit at trapped exit and powerless — the derivation correctly produces near-maximal target-side directionality for a group that neither benefits from nor controls the print-mediated dispute yet bears its downstream violence.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the coalition as tangled_rope rather than pure rope or pure snare prevents two mislabelings: treating the genuine, valuable coordination achievement (mass vernacular theological distribution, a real public good for literate participants) as if it were costless, and treating the Church's loss as if it were purely illegitimate rent-protection with no coordination counterpart on the reformist side. Both the coordination function (real distribution problem solved) and the asymmetric extraction (authority and revenue moved from one hierarchy to another, with peasants and rival printers absorbing externalized costs) are structurally present and required to be named for tangled_rope, per the gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_determinism_locus,
    'Is the operative causal locus of the Reformation''s rapid spread the reformers'' and printers'' deliberate strategic choices (this reading), the press technology''s inherent distributional properties (technological_determinism_reading), or an inseparable co-evolution of both (co_constitution_reading)?',
    'Comparative case analysis: regions/periods where equivalent print technology existed without a comparably organized reformist coalition, versus regions where a coalition existed but print access was constrained, would help isolate which factor was load-bearing. This is likely irresolvable to full satisfaction given the historical record''s entanglement.',
    'If technological properties were sufficient regardless of agency, this reading''s claimed_type and beneficiary/victim structure would collapse toward the determinist reading''s scaffold-as-mountain framing; if agency was necessary and sufficient, this reading''s tangled_rope classification is the load-bearing one for the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agency_vs_determinism_locus, conceptual, 'The kernel''s central causal-locus dispute between agency and determinism readings.').

omega_variable(
    founding_problem_corroboration_asymmetry,
    'Does the corroboration for founding_problem_status=dead (Church polemicists, modern book historians) itself reflect a confessionally biased historiographic tradition that has its own beneficiary interests in the agency narrative?',
    'Cross-check against Ottoman, Orthodox, or other non-Western-Christian print histories where no equivalent reformist coalition existed, to see whether the bypass-function-achieved-then-decayed pattern replicates independent of the specific confessional dispute.',
    'If the pattern does not replicate, the dead-founding-problem reading may itself be an artifact of Protestant historiographic self-narration rather than a neutral empirical finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_corroboration_asymmetry, empirical, 'Whether the corroborating historiography is itself beneficiary-adjacent.').

omega_variable(
    reformer_printer_symmetry_ambiguity,
    'Within the reformer-printer coalition, is the extraction genuinely mutual and symmetric, or does one party (reformers directing content, printers capturing profit) structurally dominate the other?',
    'Analysis of contract terms, patronage dependency, and relocation patterns between specific reformer-printer pairs (e.g., Luther/Cranach vs. Calvin/Geneva printers) to determine bargaining asymmetry.',
    'If printers were structurally subordinate to reformist patrons (dependent on continued doctrinal favor for market access), the coalition itself contains an internal tangled_rope rather than being a clean symmetric partnership — this would refine but not overturn the outer classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_printer_symmetry_ambiguity, empirical, 'Internal power symmetry within the beneficiary coalition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 1517, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1517, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement(tech_tr_t1530, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1530, 0.22).
narrative_ontology:measurement(tech_tr_t1545, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1545, 0.3).
narrative_ontology:measurement(tech_tr_t1560, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1560, 0.35).
narrative_ontology:measurement(tech_tr_t1580, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1580, 0.38).
narrative_ontology:measurement(tech_tr_t1600, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1600, 0.4).

% Extraction over time
narrative_ontology:measurement(tech_be_t1517, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1517, 0.42).
narrative_ontology:measurement(tech_be_t1530, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1530, 0.52).
narrative_ontology:measurement(tech_be_t1545, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1545, 0.6).
narrative_ontology:measurement(tech_be_t1560, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1560, 0.64).
narrative_ontology:measurement(tech_be_t1580, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1580, 0.67).
narrative_ontology:measurement(tech_be_t1600, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1600, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1517, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1517, 0.35).
narrative_ontology:measurement(tech_su_t1530, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1530, 0.45).
narrative_ontology:measurement(tech_su_t1545, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1545, 0.5).
narrative_ontology:measurement(tech_su_t1560, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1560, 0.53).
narrative_ontology:measurement(tech_su_t1580, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1580, 0.54).
narrative_ontology:measurement(tech_su_t1600, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1600, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__beneficiary_agency_reading, 0.12).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, co_constitution_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the technology_reformation_causality kernel. beneficiary_agency_reading (this file) authors the reformer-printer coalition as a tangled_rope with ε derived from the value of bypassed Church authority. technological_determinism_reading authors the press itself as a near-mountain (inevitable diffusion given the technology's cost structure) with minimal agency-dependent ε. co_constitution_reading authors a distributed, lower-ε rope/scaffold hybrid where neither technology nor agency alone is load-bearing. All three share the historical episode but decompose per the ε-invariance principle: measuring 'the Reformation's causal engine' by agency versus by technological property versus by co-evolution yields three different ε values, hence three constraints, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
