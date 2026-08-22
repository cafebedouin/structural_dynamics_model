% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Statutory Credential Requirement as Incumbent Rent Extraction
 *   domain: economic/regulatory/labor
 *
 * SUMMARY:
 *   This is ONE READING of the contested kernel 'licensing_statute_mandate.'
 *   This reading instantiates the RENT-SEEKING-SUPPRESSION frame: statutory
 *   credential requirements are mechanisms by which incumbent practitioners
 *   capture the regulatory board and use the state's enforcement power to
 *   restrict labor supply, suppress entry, elevate prices, and extract rents.
 *   The constraint is authored from the structural perspective that the
 *   founding rationale (consumer safety) has been satisfied by market
 *   mechanisms, reputation systems, and liability law—the high statutory
 *   barriers now persist primarily to suppress competition and extract rents.
 *   This is NOT a claim that licensing has no safety function; it is a claim
 *   that this particular constraint's primary effect is extraction, justified
 *   by reference to a problem that is substantially solved. The constraint
 *   operates as a Snare: beneficiaries (incumbent practitioners) extract from
 *   victims (qualified entrants blocked from labor markets, price-sensitive
 *   consumers facing artificially elevated service costs). The measurement
 *   series shows extractiveness rising from 0.62 to 0.81 as barriers tighten
 *   and supply compression intensifies, with theater ratio rising moderately
 *   (35% to 48%)—indicating that safety justifications are increasingly
 *   performative relative to the actual function of supply suppression.
 *
 * KEY AGENTS:
 *   - incumbent_practitioners: Organized beneficiaries (power=organized, exit=arbitrage) who control the licensing board through professional associations and lobby for higher barriers; they capture the extraction directly through restricted supply and elevated prices.
 *   - qualified_entrants: Moderate-power victims (exit=constrained) barred from entry by barriers exceeding necessary competence thresholds; the constraint suppresses their labor supply and earning potential.
 *   - price_sensitive_consumers: Powerless victims (exit=trapped) paying artificially elevated service prices due to supply restriction; they absorb the extraction indirectly.
 *   - regulatory_board: Institutional agenda-setter aligned with incumbents through board composition and budget dependence; administers the constraint and raises barriers over time.
 *   - consumer_advocates: Powerless observers outside the constraint's benefiting circle; they advocate for reform but lack institutional power to change the statute.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.81).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.72).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.81).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Statutory Credential Requirement as Incumbent Rent Extraction").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "economic/regulatory/labor").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '96d792b7-7ae2-4d4d-8f73-c36e5e38d515').
narrative_ontology:cs_kernel_codification('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', fixed_text).
narrative_ontology:cs_authority_grounding('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', extraction).
narrative_ontology:cs_interpretation_layer_present('96d792b7-7ae2-4d4d-8f73-c36e5e38d515').
narrative_ontology:cs_reading_relation('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', foundational, founding_safety_problem_is_substantially_solved).
narrative_ontology:cs_axiom_status(founding_safety_problem_is_substantially_solved, holdable).
narrative_ontology:cs_axiom_grounding('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', founding_safety_problem_is_substantially_solved, empirically_contingent).
narrative_ontology:cs_axiom('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', foundational, incumbent_professional_control_drives_barrier_height).
narrative_ontology:cs_axiom_status(incumbent_professional_control_drives_barrier_height, holdable).
narrative_ontology:cs_axiom_grounding('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', incumbent_professional_control_drives_barrier_height, empirically_contingent).
narrative_ontology:cs_reference_frame('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', consumer_protection_regime_early_professionalization).
narrative_ontology:cs_drift_state('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', contemporary_post_market_maturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('96d792b7-7ae2-4d4d-8f73-c36e5e38d515', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, qualified_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, price_sensitive_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, regulatory_board).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, incumbent_professional_superiority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Licensed practitioners who control the licensing board through professional associations, set credential standards, and defend them through testimony, lobbying, and operational control of examination design. They benefit directly from restricted supply pushing up service prices and earning rents on scarcity. They maintain the constraint through legislative capture and board administration.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners, agenda_setter,
    organized, generational, arbitrage, national).

% Individuals who possess the skill and knowledge to practice the profession (often demonstrated through apprenticeship, prior work, or alternative education) but are barred from entry by statutory requirements that exceed what is necessary for consumer protection. They must either pursue costly and time-consuming credentials that exceed their actual competence level, relocate to jurisdictions with looser requirements, or leave the profession entirely. The barrier suppresses their exit options by making the credential the only legal path.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, qualified_entrants, payer,
    moderate, biographical, constrained, national).

% Consumers who need the service but face artificially elevated prices due to restricted supply of practitioners. They absorb the rent extraction indirectly through higher service costs. Many forgo the service entirely when price exceeds their willingness to pay, creating a welfare loss that accrues to neither the incumbents nor the entrants but vanishes from the market.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, price_sensitive_consumers, payer,
    powerless, immediate, trapped, local).

% The state licensing board that administers credentialing, sets standards, and enforces the statute. Its legitimacy and budget depend on the licensing regime continuing to exist. Board members are often drawn from the incumbent profession itself, creating structural alignment with restriction. The board's institutional incentive is to maintain complexity and raise barriers, ensuring continued justification for its existence.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, regulatory_board, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, regulatory_board, beneficiary).

% Employers, industry associations, and educators who would offer certifications or training that could substitute for statutory credentials are legally barred from claiming equivalence or allowing their graduates to practice. Their exclusion is what the enforcement machinery exists to maintain—they could compete on cost and speed if permitted to signal competence.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, alternative_credential_providers, excluded,
    moderate, biographical, trapped, national).

% Advocacy organizations, policy analysts, and economists outside the profession who analyze credential requirements and advocate for reduced barriers. They have limited power to change the constraint and must work through legislative channels dominated by incumbent lobbying. They observe the rent extraction but lack the institutional position to alter it directly.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumer_advocates, observer,
    powerless, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, there is NO genuine coordination function—the statute's only effect is to limit who can practice and at what price. The public-safety reading would claim coordination (ensuring competent practitioners); this reading rejects that frame and asserts the safety rationale as post-hoc justification for restriction.
% TRANSFER_FUNCTION: Moves wealth from price-sensitive consumers (through higher service prices) and from qualified entrants (through exclusion from the market) to incumbent practitioners and the licensing board (through reduced supply and market control). The transfer is mediated by the artificial scarcity the statute creates.
% ABSENT_VOICES: Qualified entrants without the statutory credential are structurally excluded from the licensing board and legislative process; their voice would argue for recognition of alternative pathways. Consumers in the price-sensitive segment are powerless and rarely organized into testimony; their voice would claim harm from elevated prices. Alternative credential providers are barred from the conversation entirely by enforcement of the statute itself.
% DISAPPEARANCE_RATIONALE: If the statute and its enforcement vanished, the incumbent profession would lose its exclusive supply control. New practitioners would enter rapidly, service prices would decline as competition intensified, and the distribution of practitioners would shift toward geographic and price segments currently underserved. Incumbent earnings would compress. The profession would reorganize around reputation and employer-verified competence instead of statutory barriers.
% FOUNDING_PROBLEM: Historical establishment of the profession claimed consumer protection—preventing incompetent or unsafe practitioners from harming the public. The founding problem was stated as 'dangerous practitioners exploit consumer information asymmetry.'
% FOUNDING_PROBLEM_CORROBORATION: Incumbent practitioners and regulatory boards attest the problem is live and justify high barriers as necessary for safety. Economists, policy analysts outside the profession, and comparative studies of jurisdictions with lower barriers attest the founding problem is substantially solved by reputation, liability, employer screening, and market mechanisms—the high barriers persist despite the founding problem's resolution. Legislative testimony from consumer advocates documents rising prices without corresponding safety improvements. Economic analysis shows no correlation between barrier height and safety outcomes across jurisdictions.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the statute's measurable effect is artificial scarcity and price elevation decoupled from competence-ensuring function. Safety requirements exist (e.g., exam scores, insurance), but the statutory barriers exceed what is necessary—comparative data from lower-barrier jurisdictions show equivalent safety outcomes, indicating the high barriers extract without proportional safety gain. Suppression is high (0.72) because entry is legally barred; alternatives (employer certification, apprenticeship, reputation) are excluded by statute enforcement; the constraint's persistence depends on active legal suppression of exit paths. Theater ratio is moderate-high (0.48) and rising, indicating that board activity increasingly emphasizes complexity and procedure (licensing exam difficulty, credential hour requirements) beyond what safety requires—the theater is the machinery of restriction dressed as quality assurance. Accessibility_collapse is moderate-high (0.68) because alternatives are structurally foreclosed by law; once the statute is understood, entrants' options collapse to statutory compliance. Resistance is moderate (0.54) because qualified entrants and consumer advocates push back through legislative channels, but incumbent lobbying dominates. The measurement series runs on one shared grid: all three metrics measured at every time point (0, 5, 10, 15, 20, 25, 30, 40), showing extractiveness and suppression rising early and plateauing as the constraint matures, with theater_ratio rising throughout as justification becomes more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (incumbents controlling the board) experiences this as legitimate professional self-regulation with safety function; the constraint computes as rope-like coordination to them. The payer seats (entrants, consumers) experience it as suppression and extraction; the constraint computes as snare-like from their position. The engine computes these divergent classifications from the structural data—differing power, exit options, and beneficiary/victim positions—without reconciling them. The committer frame (this reading) asserts the snare classification is structurally true: the constraint's primary function is extraction via supply suppression, justified by a founding problem that is substantially solved.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent_practitioners are beneficiaries: they capture the extraction directly (d near 0.0, full beneficiary end). Qualified_entrants are targets: they are barred from labor markets and face suppression (d near 1.0, full target end). Price_sensitive_consumers are targets: they pay artificially elevated prices (d near 1.0, full target end). The regulatory_board has a secondary_role as both agenda_setter and beneficiary because its institutional survival depends on the licensing regime continuing; the board is structurally aligned with incumbents (d moderate, ~0.3). Consumer_advocates are observers: they have analytical standing but no material position in the constraint (d = 0.5, symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy: the founding mandate was 'prevent incompetent practitioners from harming consumers.' That problem is substantially solved by modern liability, reputation, employer screening, and market mechanisms. Comparative evidence (jurisdictions with lower barriers show equivalent safety outcomes) indicates the founding problem is dead or nearly dead. Yet the constraint persists and intensifies (extractiveness rises from 0.62 to 0.81; suppression plateaus high at 0.72; theater_ratio rises to 0.48). The persistence is sustained by incumbent capture of the regulatory board—the constraint has ceased to be a public-safety mechanism and become an extraction mechanism masked as quality assurance. The rising theater_ratio (35% to 48%) tracks the increasingly elaborate justification machinery: licensing exams become harder, hour requirements multiply, continuing education expands—these add cost without measurable safety gain. Mandatrophy resolution is partial: the constraint's PUBLIC mandate (safety) is dead; its PRIVATE mandate (supply restriction) is live and intensifying. The snare classification is driven by this decay: the constraint now extracts without proportional coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_necessity_boundary,
    'What is the minimum level of statutory credential requirement necessary to prevent consumer harm, versus the current requirement level?',
    'Comparative analysis of safety outcomes (adverse event rates, malpractice claims, consumer complaints) across jurisdictions with varying barrier heights; pilot programs that reduce barriers and measure safety changes; economic analysis of marginal safety improvement per credential increment.',
    'If current barriers significantly exceed the safety minimum, the surplus extraction is confirmed and the snare classification is supported. If current barriers match the safety minimum, the public-safety and rent-seeking readings cannot be disentangled structurally—the constraint serves both functions, making it tangled_rope rather than pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safety_necessity_boundary, empirical, 'Whether statutory credential requirements exceed the competence threshold necessary for consumer protection.').

omega_variable(
    incumbent_capture_mechanism,
    'To what degree does incumbent professional control over board composition, examination design, and credential standards drive the constraint''s barriers?',
    'Analysis of board membership (percentage from the regulated profession vs. outside appointments); examination history (do exam pass rates and difficulty track safety metrics or do they track incumbent supply goals); legislative testimony from board members and incumbents; jurisdictions with independent boards vs. incumbent-controlled boards.',
    'Evidence of strong incumbent control would confirm the snare reading (extraction via supply suppression under cover of regulation). Evidence of weak incumbent control would suggest the barrier height is determined by genuine safety standards, not capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capture_mechanism, empirical, 'Whether the constraint''s barriers reflect incumbent preferences for supply restriction versus public-safety requirements.').

omega_variable(
    alternative_quality_assurance_viability,
    'Could reputation, liability law, employer screening, professional voluntary certification, and insurance adequately ensure competence without statutory barriers?',
    'Comparative study of fields with no statutory licensing (many consulting, trading, analysis roles) and their safety/competence outcomes; historical analysis of professional fields before statutory licensing (did quality collapse, or was quality maintained through reputation and market mechanisms?); jurisdictions experimenting with title-only licensing or voluntary certification.',
    'Evidence that alternative mechanisms are viable would establish the statutory requirement as non-essential and support the extraction reading. Evidence that alternatives fail would suggest the statute serves a genuine coordination function that the snare reading misses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_quality_assurance_viability, empirical, 'Whether non-statutory mechanisms (reputation, liability, employer screening) can substitute for statutory credential requirements without increasing consumer harm.').

omega_variable(
    reading_contestation_boundary,
    'Is the distinction between the public_safety_coordination reading and the rent_seeking_suppression reading structurally resolvable, or is it intrinsically contestable based on differing values about the balance between protection and freedom?',
    'If the empirical questions (safety_necessity_boundary, incumbent_capture_mechanism, alternative_quality_assurance_viability) are resolved and safety can be maintained without high barriers, the readings diverge empirically—the snare reading is confirmed. If safety requires high barriers AND incumbents happen to benefit, the contestation reduces to a values question (is regulation-for-safety justified even if incumbents profit?) and is not empirically resolvable.',
    'If the readings are empirically resolvable, the snare classification stands. If the readings are intrinsically contestable (both empirically coherent, diverging only on values), the constraint should carry an additional omega documenting the value disagreement and the reading_relations should shift from coexists_with to a new type indicating asymmetric framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contestation_boundary, conceptual, 'Whether the rent-seeking vs. safety-coordination distinction is empirically resolvable or intrinsically contestable.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (legal barriers, enforcement machinery) or internalized (entrants believe they cannot compete, have accepted the legitimacy of high barriers)?',
    'Post-barrier removal trajectory: if suppression persists after legal barriers are removed (entrants still avoid entry, or move away, or remain convinced high credentials are necessary), the suppression is internalized. If suppression disappears and entry accelerates upon barrier removal, suppression is structural.',
    'If suppression is internalized, the constraint''s effective extractive power is higher than the structural measure suggests—the target carries the suppression even after the barrier is removed. If suppression is structural, removing the barrier removes the constraint''s effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the suppression (exclusion from labor markets) is structurally enforced or internalized by the target population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(lice_tr_t0, observed).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 5, 0.36).
narrative_ontology:measurement_basis(lice_tr_t5, observed).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(lice_tr_t10, observed).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 15, 0.43).
narrative_ontology:measurement_basis(lice_tr_t15, observed).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(lice_tr_t20, observed).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 25, 0.47).
narrative_ontology:measurement_basis(lice_tr_t25, observed).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(lice_tr_t30, observed).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(lice_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(lice_be_t0, observed).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 5, 0.66).
narrative_ontology:measurement_basis(lice_be_t5, observed).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 10, 0.7).
narrative_ontology:measurement_basis(lice_be_t10, observed).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 15, 0.74).
narrative_ontology:measurement_basis(lice_be_t15, observed).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 20, 0.77).
narrative_ontology:measurement_basis(lice_be_t20, observed).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 25, 0.79).
narrative_ontology:measurement_basis(lice_be_t25, observed).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(lice_be_t30, observed).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(lice_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(lice_su_t0, observed).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(lice_su_t5, observed).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(lice_su_t10, observed).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(lice_su_t15, observed).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(lice_su_t20, observed).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(lice_su_t25, observed).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(lice_su_t30, observed).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(lice_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__rent_seeking_suppression, 0.06).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'licensing_statute_mandate.' The kernel is a statutory credentialing regime whose legitimacy is grounded in consumer protection (lineage authority: the 19th–20th-century professionalization movement). The three readings instantiate structurally distinct constraints: (1) public_safety_coordination reads the statute as solving consumer information asymmetry and preventing incompetent practitioners; (2) graduated_access_filter reads the statute as creating tiered market access that sorts entrants by class and resource; (3) rent_seeking_suppression (THIS reading) reads the statute as a mechanism by which incumbents restrict supply and extract rents. All three readings share the same fixed text (statute) but differ in their account of the constraint's primary function and beneficiary/victim structure. This reading asserts the public-safety founding problem is substantially solved (dead status) and the constraint now operates as pure extraction (snare). The public_safety_coordination reading would argue the problem is live and the constraint serves genuine coordination. The graduated_access_filter reading would argue the constraint's primary effect is sorting entrants by prior advantage, not restriction per se. Each reading is a self-contained constraint story with its own ε, stakeholder set, and type classification. They are linked here via network.affects_constraints to indicate their shared kernel and structural relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
