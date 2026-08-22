% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary (Enclosure Reading)
 *   domain: intellectual_property/information_governance
 *
 * SUMMARY:
 *   The enclosure reading interprets the statutory derivative work boundary
 *   as a prophylactic gate: ANY use of copyrighted expression in creating new
 *   work constitutes preparation of a derivative work, triggering a licensing
 *   requirement BEFORE creation begins. This reading is one instantiation of
 *   a contested kernel (the statutory definition of derivative work itself).
 *   Under this reading, downstream creators face a licensing bottleneck
 *   before they can legally create. The reading is structurally
 *   high-extraction (snare): licensing is enforced pre-creation, alternatives
 *   are collapsed (fair use becomes a post-creation defense rather than a
 *   pre-creation exemption), and the rents flow to copyright incumbents and
 *   licensing intermediaries. A competing coordination reading would permit
 *   transformative uses and intermediate adaptations without licensing; a
 *   hybrid reading would permit non-commercial transformation while requiring
 *   authorization for commercial derivative creation. This story instantiates
 *   ONLY the enclosure reading as a constraint with stable, high ε.
 *
 * KEY AGENTS:
 *   - copyright_incumbents: institutional beneficiaries — set and enforce the derivative-work gate through licensing requirements; collect licensing rents
 *   - downstream_creators: powerless victims — face licensing requirements before creation; cannot afford licensing; legally trapped
 *   - transformative_artists: moderate-power victims with identity-lock — practice is inherently transformation; exit means abandoning creative practice
 *   - licensing_intermediaries: institutional beneficiaries — profit from licensing administration and enforcement
 *   - competition_authorities: institutional observers — can mandate licensing remedies but are not party to the licensing system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.81).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.76).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary (Enclosure Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property/information_governance").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, 'c4e27766-fa00-4afc-916c-8b8bd046a7fc').
narrative_ontology:cs_kernel_codification('c4e27766-fa00-4afc-916c-8b8bd046a7fc', fixed_text).
narrative_ontology:cs_authority_grounding('c4e27766-fa00-4afc-916c-8b8bd046a7fc', extraction).
narrative_ontology:cs_interpretation_layer_present('c4e27766-fa00-4afc-916c-8b8bd046a7fc').
narrative_ontology:cs_reading_relation('c4e27766-fa00-4afc-916c-8b8bd046a7fc', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4e27766-fa00-4afc-916c-8b8bd046a7fc', derivative_work_statutory_boundary__hybrid_carveout_reading, influences).
narrative_ontology:cs_axiom('c4e27766-fa00-4afc-916c-8b8bd046a7fc', foundational, any_expression_use_triggers_licensing).
narrative_ontology:cs_axiom_status(any_expression_use_triggers_licensing, holdable).
narrative_ontology:cs_axiom_grounding('c4e27766-fa00-4afc-916c-8b8bd046a7fc', any_expression_use_triggers_licensing, deontological).
narrative_ontology:cs_axiom('c4e27766-fa00-4afc-916c-8b8bd046a7fc', secondary, pre_creation_authorization_requirement_doctrine).
narrative_ontology:cs_axiom_status(pre_creation_authorization_requirement_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c4e27766-fa00-4afc-916c-8b8bd046a7fc', pre_creation_authorization_requirement_doctrine, conventional).
narrative_ontology:cs_reference_frame('c4e27766-fa00-4afc-916c-8b8bd046a7fc', copyright_protection_via_derivative_gating).
narrative_ontology:cs_drift_state('c4e27766-fa00-4afc-916c-8b8bd046a7fc', contemporary_licensing_market_capture, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c4e27766-fa00-4afc-916c-8b8bd046a7fc', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, copyright_incumbents).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, downstream_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, transformative_artists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, independent_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, copyright_protection_requires_prophylactic_licensing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major copyright holders and their licensing administrators. Control the gate through which downstream creators must pass before creating derivative-adjacent works. Enforce the enclosure reading via licensing agreements, takedown notices, and litigation. Collect licensing rents on derivative authorization. Justify the boundary as protecting expression-level investments and ensuring compensation for original authors.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, copyright_incumbents, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Independent creators, remix artists, parody writers, and transformative works producers. Face licensing requirements BEFORE creation begins under the enclosure reading. Cannot legally prepare new work incorporating any copyrighted expression without pre-authorized consent. Must license or abandon ideas. No alternative distribution channels exist that escape copyright territorial reach.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, downstream_creators, payer,
    powerless, biographical, trapped, global).

% Software developers, game modders, and tool builders. Create value by modifying or extending existing codebases and assets. Under the enclosure reading, any use of copyrighted code or assets in creating derivative software requires pre-authorized licensing. They benefit from open platforms and communities that permit derivative work; they pay licensing fees or face takedown and litigation when pursuing downstream innovation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, independent_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, independent_developers, beneficiary).

% Visual artists, musicians, and writers whose practice is inherently transformative — riffing on, critiquing, or reimagining existing cultural material. The enclosure reading criminalizes the creative act itself as 'preparation of derivative work' before fair use or transformative analysis can shield the final work. Identity is fused with practice; exit means abandoning the creative project itself.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, transformative_artists, payer,
    moderate, biographical, identity_locked, global).

% Rights-management agencies, licensing clearinghouses, and collective rights administrators. Collect licensing fees on behalf of copyright holders and for themselves. Profit from the enclosure reading by enforcing gating and managing the licensing bottleneck. Incentivized to maximize licensing requirements and minimize licensing exemptions.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries, beneficiary,
    powerful, civilizational, arbitrage, global).

% Antitrust and competition regulators in multiple jurisdictions. Investigate whether the enclosure reading and its enforcement create barriers to entry in derivative markets and foreclose competition. Their authority to mandate remedies (compulsory licensing, carve-outs, safe harbors) could alter the constraint's enforcement and extraction.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% The four-factor test that permits some uses of copyrighted expression without authorization. Under the enclosure reading, fair use applies AFTER creation is complete and infringement has already occurred — the doctrine is a defense, not a licensing exemption. The timing shift moves the burden of proof and cost to downstream creators, collapsing fair use's protective function.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine, excluded,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine).

% Libraries, archives, and digital preservation organizations. Excluded from the enclosure system's logic: they have no stake in derivative licensing but bear the cost of copyright term expansion and derivative-use restrictions that prevent them from creating derivative works for preservation, restoration, and accessibility.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, public_domain_archivists, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, copyright_incumbents).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures copyright holders can control and monetize adaptations and extensions of their original expression. Provides a single licensing gate through which adaptation rents flow back to original authors.
% TRANSFER_FUNCTION: Moves licensing revenue from downstream creators (who must pay for authorization to incorporate copyrighted expression) to copyright incumbents and licensing intermediaries. The transfer is enforced by legal threat: creation without authorization constitutes infringement.
% ABSENT_VOICES: Creators practicing under alternative copyright regimes (copyleft, creative commons, dedication to public domain) would object that the enclosure reading conflates all expression uses into a single licensing category and ignores creator intent. They are excluded from the licensing gate design but bound by its enforcement. Independent creators in jurisdictions without robust fair use traditions would testify that the enclosure reading makes derivative creation legally impossible, not just expensive.
% DISAPPEARANCE_RATIONALE: If the enclosure reading disappeared overnight and derivative-work licensing were no longer required before creation, downstream creators would move freely into adaptation markets. Licensing revenue to copyright holders would evaporate or contract sharply. The licensing intermediary industry would collapse. Software modification ecosystems, mashup cultures, and transformative art practices would shift to non-infringing status without authorization delays. The software, music, film, and visual art economies would reorganize around derivative-work freedom.
% FOUNDING_PROBLEM: Original expression creators invest in generating protectable work and deserve compensation when others build derivative versions. Without control over derivatives, original creators cannot capture the value of adaptation-enabling ideas they generated.
% FOUNDING_PROBLEM_CORROBORATION: Copyright incumbents attest the founding problem is still live, citing lost licensing revenue to unauthorized derivatives. Downstream creators and independent analysis from competition authorities attest the founding problem is substantially solved by established licensing markets and that the enclosure reading's pre-creation gating extracts rents beyond compensation for original work. Legislative testimony and economic analyses from non-benefiting parties establish that licensing requirements have shifted from protecting expression to controlling derivative markets.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.62 to 0.81 across the interval. The initial value reflects that licensing markets exist (rents are not absolute predation) but licensing requirements are expanding through case law and enforcement. By year 35, the metric plateaus at 0.81: the enclosure reading has become standard practice, licensing requirements are comprehensive, and the extraction is stable at near-maximal levels because downstream creators have adapted to licensing (the behavioral shift from 'ask permission' resistance to normalized licensing reduces active resistance, lowering suppression-requirement slope). Suppression requirement (0.58 to 0.76) models the enforcement infrastructure maturation: early-period enforcement relies on litigation and takedown notices; later periods shift to routine licensing administration, normalization, and institutional compliance. Theater ratio (0.12 to 0.28) models increasing performative character: early licensing claims that copyright protection requires pre-creation authorization; later periods increasingly maintain licensing rents (extraction divorced from stated protection function) through licensing intermediary theater and administrative ceremony. The measurement series report extraction accumulation under prophylactic framing — OQ-26 classic Mandatrophy pattern: founding problem (protecting original expression) solved by market licensing; constraint persists by shifting to enclosure (controlling derivative markets).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (copyright incumbents) and payers (downstream creators) compute radically different type classifications from the same structural facts. From the incumbent seat, the enclosure reading is a coordination mechanism protecting original investment and ensuring compensation flow — they compute cooperative/rope-flavored classification. From the payer seat (downstream creator), the same pre-creation licensing gate is enforced extraction, collapsing alternatives and trapping them in licensing rents — they compute snare. The engine derives this divergence from the asymmetric beneficiary/victim structure, the institutional power difference (institutional vs. powerless), and the exit-option collapse (incumbents have arbitrage; downstream creators are trapped). The committer frame (this is ONE reading of a contested kernel) amplifies the divergence: the coordination reading would distribute benefits and costs differently; the enclosure reading concentrates benefits in incumbents and costs in creators.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright incumbents have d ≈ 0.05 (full beneficiary end): they set the licensing gate, enforce it, collect rents with no licensing cost to themselves, and have arbitrage-grade exit (can license or self-exploit). Downstream creators have d ≈ 0.92 (near-full target end): they bear licensing costs, face legal/economic pressure to comply before creation, are trapped with no alternative distribution, and identity-locked transformative artists cannot exit without abandoning practice. Licensing intermediaries have d ≈ 0.12 (beneficiary): they profit from licensing administration without bearing creation costs. The directionality override for transformative_artists raises d slightly (0.92 → 0.94) to account for identity-lock specificity: even mobile creators cannot exit; the identity lock binds to the practice itself, not external circumstances.
 *
 * MANDATROPHY ANALYSIS:
 *   The enclosure reading exhibits the classic Mandatrophy II pattern: founding problem (original creators need compensation for expression) is solved by market licensing; persistence is now enforced by the derivative-work gate itself (licensing requirement pre-creation), not by the economic need to compensate. Theater-ratio accumulation (0.12 → 0.28) and the extraction plateau (0.81) model the phase transition: as licensing becomes institutionalized, enforcement shifts from need-based (protecting real investment) to pure gate-keeping (controlling downstream markets). The constraint's mandate has outlived its function; the pre-creation licensing requirement extracts rents beyond expression protection. Base_properties.mandatrophy_resolved is false here because the reading actively denies mandatrophy — enclosure doctrine asserts that pre-creation licensing IS the protection function — but the measurement series and the theater-ratio accumulation signal the divergence to the mandatrophy detection system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophylactic_vs_substantive_licensing,
    'Does the enclosure reading''s pre-creation licensing requirement protect expression by channeling compensation, or does it extract rents by controlling derivative markets beyond expression protection?',
    'Empirical: compare licensing revenue flows to original creators (compensation theory) versus licensing revenue flows to intermediaries and enforcement machinery (capture theory). Conceptual: examine whether licensing requirements correlate with downstream innovation in derivative genres or whether they suppress innovation regardless of compensation outcomes.',
    'If licensing channeling compensation is the real function, the extraction is part of the coordination cost and ε should be lower (rope instead of snare). If licensing primarily controls markets and intermediary rents dominate, ε remains high and mandatrophy analysis holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prophylactic_vs_substantive_licensing, empirical, 'Whether licensing requirements protect expression or control derivative markets.').

omega_variable(
    fair_use_timing_inversion,
    'Is fair use a licensing exemption (applied pre-creation, guiding what can be created without authorization) or a defense (applied post-creation, protecting a creator from infringement liability after creation)?',
    'Statutory or case-law clarification: does copyright law permit creators to rely on fair use before creation (avoiding the enclosure gate), or must creators assume infringement risk and defend in litigation? Jurisdictional comparison: some countries apply fair-use-equivalent doctrine before licensing; others apply it only post-creation.',
    'If fair use is a pre-creation exemption, the enclosure reading collapses and derivative creation is possible without licensing (snare → rope). If fair use is post-creation only, the enclosure reading persists and the licensing gate is effective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fair_use_timing_inversion, conceptual, 'Whether fair use operates as pre-creation exemption or post-creation defense.').

omega_variable(
    identity_lock_transformative_exit,
    'Can transformative artists practicing under the enclosure reading exit by abandoning the transformative practice, or is the identity fusion absolute?',
    'Post-licensing-change observation: if jurisdictions permit derivative creation without licensing, do transformative artists remain in the field, or do many exit/shift to non-derivative work? Post-identity-reframing observation: if artists reframe identity away from transformation, do they continue creating (in different modes)?',
    'If artists can exit by identity reframing, exit_options should be raised from identity_locked to constrained; if the identity is absolute and departure is unthinkable regardless of legal change, identity_lock holds and d remains near 0.94.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_transformative_exit, empirical, 'Whether identity-locked transformative practice permits identity exit.').

omega_variable(
    enclosure_vs_coordination_framework_logics,
    'Is the enclosure reading logically foreclosed by the coordination reading, or do they coexist as different but compatible interpretations of the derivative-work statute?',
    'Statutory grammar examination: does the statute''s text permit both readings to be held by different parties simultaneously, or does one reading''s core premise logically contradict the other such that no single jurisdiction could hold both? Case-law genealogy: have courts explicitly endorsed one reading over another as the binding interpretation, or do different courts apply different readings?',
    'If foreclosed (one reading rules out the other), the reading_relations should declare ''forecloses''. If coexistent (both readings live in different jurisdictions or parties), reading_relations should declare ''coexists_with''. If the coordination reading creates downstream pressure on enclosure (e.g., licensing pools and compulsory licenses structured to permit coordination), reading_relations should declare ''influences''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enclosure_vs_coordination_framework_logics, conceptual, 'Logical relationship between enclosure and coordination readings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.76 at interval end) structural (legal threat, licensing cost, economic barriers to creation) or internalized (creators have internalized the licensing requirement as legitimate, constraining their behavior even absent enforcement)?',
    'Post-enforcement relaxation: if licensing requirements were removed but copyright remained enforceable post-creation via fair use defense, would downstream creators continue to seek licensing (internalized suppression persists) or freely create (suppression was structural)? Comparative observation: creators in jurisdictions with weak copyright enforcement versus strong enforcement.',
    'If suppression is entirely structural, it remains external and modifiable by legal change. If partly internalized, the constraint carries behavioral lock-in that persists even after enforcement changes; creators would need to reframe their understanding of legitimate creation to exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(deri_tr_t0, observed).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(deri_tr_t5, observed).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(deri_tr_t10, observed).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(deri_tr_t15, observed).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(deri_tr_t20, observed).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(deri_tr_t25, observed).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(deri_tr_t30, observed).
narrative_ontology:measurement(deri_tr_t35, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(deri_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(deri_be_t0, observed).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement_basis(deri_be_t5, observed).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(deri_be_t10, observed).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement_basis(deri_be_t15, observed).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(deri_be_t20, observed).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement_basis(deri_be_t25, observed).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(deri_be_t30, observed).
narrative_ontology:measurement(deri_be_t35, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 35, 0.81).
narrative_ontology:measurement_basis(deri_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(deri_su_t0, observed).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(deri_su_t5, observed).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(deri_su_t10, observed).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(deri_su_t15, observed).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(deri_su_t20, observed).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement_basis(deri_su_t25, observed).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(deri_su_t30, observed).
narrative_ontology:measurement(deri_su_t35, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement_basis(deri_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__enclosure_reading, 0.12).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% The derivative-work boundary is a contested kernel instantiated by three structurally distinct constraints. The enclosure_reading (this file) interprets the boundary as ANY expression use = derivative work = licensing required; it produces high-extraction snare. The coordination_reading interprets the boundary as only substantial recastings with incorporated protection = derivative work; it produces moderate-extraction rope. The hybrid_carveout_reading divides the boundary by commercial intent: non-commercial transformation permitted, commercial use requires licensing; it produces moderate-extraction tangled_rope. The three readings share the kernel (statutory definition) but diverge on ε, beneficiary structure, and type. Ε-invariance principle: each reading instantiates a single ε value because each reading assesses the referent (the standing derivative-work arrangement) by its own lights. The enclosure reading sees pre-creation licensing as essential protection; the coordination reading sees it as market control; the hybrid reading sees it as necessary licensing for commercial benefit. The readings are not different observable angles on a single constraint — they are genuinely different constraints emerging from different interpretations of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__enclosure_reading, moderate, 0.94).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
