% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__continuity_reading, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: orthographic_kernel__continuity_reading
 *   human_readable: Arabic Script as Ottoman Continuity Anchor
 *   domain: political/linguistic/cultural
 *
 * SUMMARY:
 *   The Ottoman Empire maintains Arabic script as the exclusive medium of
 *   state communication, religious authority, and formal education. This
 *   constraint is one reading of the orthographic kernel: the continuity
 *   reading frames Arabic script as the essential anchor preserving Ottoman
 *   cultural identity and unbroken linkage to Islamic civilization. The
 *   alternative readings — the modernization reading (script as a neutral
 *   technical tool that can be reformed without cultural rupture) and the
 *   rupture reading (script change as necessary national liberation) —
 *   represent different framings of the same institutional fact: the state's
 *   script monopoly. This constraint story instantiates the continuity
 *   reading: Arabic script's preservation is valued as non-negotiable
 *   cultural inheritance, and alternatives are treated as
 *   identity-threatening. The claim/metric gap is structural to this reading:
 *   the continuity reading claims rope-like coordination (preservation
 *   function) while the authored metrics reflect substantially extractive
 *   enforcement (suppression of alternatives, blocking of technical
 *   literacy). The engine computes the measured divergence; the reading's own
 *   logic should not reconcile it away.
 *
 * KEY AGENTS:
 *   - Ottoman religious scholars (institutional, identity-locked beneficiary) — control the religious interpretation monopoly that script unity preserves
 *   - State cultural authority (institutional, agenda-setter) — administers and enforces the orthographic standard; collects the political legitimacy gain from Islamic continuity framing
 *   - Ottoman administrative class (powerful, constrained beneficiary/payer) — retains literacy privilege but faces friction from rising technical complexity demands
 *   - Commercial and technical populations (moderate, constrained payers) — blocked from accessing faster-learning scripts or European technical documentation; absorb the cognitive/economic cost
 *   - European trading partners (powerful, excluded) — forced to use dragomans because direct written communication in accessible scripts is administratively suppressed
 *   - Reform-minded officials (moderate, excluded observers) — recognize the technical cost but are blocked by combined religious and state cultural authority pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, 0.68).
domain_priors:suppression_score(orthographic_kernel__continuity_reading, 0.76).
domain_priors:theater_ratio(orthographic_kernel__continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__continuity_reading, "Arabic Script as Ottoman Continuity Anchor").
narrative_ontology:topic_domain(orthographic_kernel__continuity_reading, "political/linguistic/cultural").

domain_priors:requires_active_enforcement(orthographic_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__continuity_reading, '9abd2f93-3e98-44ac-804c-be19b2a0bdf4').
narrative_ontology:cs_kernel_codification('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', fixed_text).
narrative_ontology:cs_authority_grounding('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', lineage).
narrative_ontology:cs_interpretation_layer_present('9abd2f93-3e98-44ac-804c-be19b2a0bdf4').
narrative_ontology:cs_reading_relation('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_reading_relation('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', foundational, script_institutional_continuity_fused).
narrative_ontology:cs_axiom_status(script_institutional_continuity_fused, holdable).
narrative_ontology:cs_axiom_grounding('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', script_institutional_continuity_fused, deontological).
narrative_ontology:cs_axiom('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', secondary, arabic_script_quranic_integrity_prerequisite).
narrative_ontology:cs_axiom_status(arabic_script_quranic_integrity_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', arabic_script_quranic_integrity_prerequisite, empirically_contingent).
narrative_ontology:cs_reference_frame('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', islamic_institutional_continuity).
narrative_ontology:cs_drift_state('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', eighteenth_century_european_technical_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9abd2f93-3e98-44ac-804c-be19b2a0bdf4', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__continuity_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ottoman_religious_scholars).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, traditional_islamic_institution).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, state_cultural_authority).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, ottoman_administrative_class).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, commercial_literacy_seekers).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, technical_military_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ottoman_administrative_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Arabic script is the vehicle for Quranic interpretation, hadith transmission, and religious jurisprudence. Maintaining it as the sole official script preserves their institutional monopoly on textual interpretation and their authority to adjudicate religious law. Their authority depends on the script's stability and exclusivity.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_religious_scholars, beneficiary,
    institutional, generational, identity_locked, regional).

% Arabic script is the gateway to the wider Islamic intellectual tradition across the Levant, North Africa, and the Middle East. Maintaining it preserves institutional continuity with that tradition and positions the Ottoman religious establishment as the custodian of Islamic knowledge. Loss of the script would sever institutional lineage.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, traditional_islamic_institution, beneficiary,
    institutional, civilizational, identity_locked, global).

% Must continue training in Arabic-script Ottoman bureaucratic practice (the traditional medium of state documents, decrees, and records) despite the rising administrative burden of managing a large empire with multiple script systems. They retain administrative authority but face increasing friction as technical literacy demands expand faster than training capacity.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_administrative_class, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, ottoman_administrative_class, beneficiary).

% Want access to European commercial correspondence, accounting systems, and technical manuals (many translated into Latin script or simple phonetic systems). They are excluded from educational pathways that would teach these systems because the state maintains Arabic script as the exclusive medium of formal learning. Access to faster, easier-to-learn scripts is administratively suppressed.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, commercial_literacy_seekers, payer,
    moderate, biographical, constrained, regional).

% Need to learn new weapons systems, military engineering, and naval technology documented in European languages and scripts. They are constrained by the state's refusal to authorize formal instruction in Latin script, forcing them to rely on clandestine or informal translation. Their operational capacity is hampered by the constraint.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, technical_military_personnel, payer,
    organized, biographical, constrained, regional).

% Sets and enforces the orthographic standard: Arabic script is the sole legal medium for all official documents, decrees, educational curricula, and public signage. Justifies this as preserving Ottoman cultural integrity and Islamic legitimacy. Actively suppresses alternative scripts through educational monopoly, legal prohibition, and clerical pressure.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, state_cultural_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% Must employ dragomans (interpreters/translators) to mediate all commercial and diplomatic correspondence because the Ottoman state will not conduct official business in Latin script or other European orthographies. Their exclusion from direct written communication with Ottoman counterparts forces a dependency structure and increases transaction costs.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, european_trading_partners, excluded,
    powerful, biographical, trapped, global).

% Recognize that the constraint is slowing Ottoman technological absorption and administrative efficiency but are blocked from advocating orthographic reform by the combined pressure of state cultural authority and religious institution. Their voice is structurally absent from the decision-making process about script.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, reform_minded_ottoman_officials, excluded,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, reform_minded_ottoman_officials, observer).

% The normative claim that maintaining Islamic institutional and textual continuity is a supreme state value benefits from the constraint's enforcement. The constraint vindicates the doctrine by making any orthographic change appear as heretical rupture rather than neutral technical reform.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, islamic_continuity_doctrine, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(orthographic_kernel__continuity_reading, islamic_continuity_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__continuity_reading, traditional_islamic_institution).
narrative_ontology:fixing_cost_class(orthographic_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified script system for state documents, religious texts, and educational materials, preventing the fragmentation that would arise if multiple writing systems operated in parallel within Ottoman administrative and cultural domains. One script = one authoritative interpretation chain for law, theology, and state communication.
% TRANSFER_FUNCTION: Transfers the cognitive and economic costs of learning one complex script system (Arabic, with its calligraphic, semantic, and historical depth) from the religious and administrative elite to the commercial and technical population. The skilled administrative class retains its monopoly on advanced literacy; access to faster-learning, technologically-adjacent scripts is blocked.
% ABSENT_VOICES: Commercial merchants who want to learn Latin script for trade efficiency; military engineers who recognize the technical liability of isolated scripts; Ottoman Jewish and Christian minorities who use Arabic script for Ottoman administrative purposes but could accelerate technical learning with script flexibility; European merchants and diplomats who would benefit from direct written communication without interpreter mediation.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, Ottoman commercial classes would absorb Latin script literacy within a generation, military technical capacity would accelerate, administrative efficiency would improve, and the empire would face the full pressure of European technological superiority without the script delay as a barrier to adoption. The institutional structure that depends on script exclusivity (the religious monopoly on interpretation, the administrative class's literacy privilege) would reorganize.
% FOUNDING_PROBLEM: Early Ottoman state needed a unified orthographic system to maintain religious legitimacy, preserve the Quran's integrity against misinterpretation, and consolidate a multiethnic empire around shared cultural symbols. Arabic script and the Islamic interpretive tradition provided that unity.
% FOUNDING_PROBLEM_CORROBORATION: Ottoman religious scholars and cultural traditionalists attest the founding problem is perpetual — Islam and Ottoman identity remain inseparable from Arabic script. Reform-minded officials and European observers attest the problem is solved — script is no longer the limiting factor to literacy or technical learning, and the constraint now blocks rather than solves coordination. Military historians note the technical cost of the script barrier became acute by the 18th century; religious historians note the barrier never prevented textual misinterpretation.
narrative_ontology:disappearance_verdict(orthographic_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint imposes a real cognitive tax on technical and commercial populations without proportional benefit — the coordination function (unified scripts for state documents) could be served by multiple scripts without loss. Suppression is higher (0.76) because the constraint's persistence depends on active enforcement through educational monopoly and clerical pressure, not on voluntary adoption. Theater ratio rises over the interval (0.22→0.42) as the Empire ages and the gap between claimed coordination function and actual suppression widens — the justification shifts from genuine (script unity prevents fragmentation) to performative (script unity preserves Islamic identity) as technical pressure mounts. The constraint rises in extractiveness through t=80 (0.48→0.69) as European technical advantage grows and the cost of Ottoman script isolation becomes clearer, then plateaus as suppression hardens to contain reform pressure. All seven time points share one grid: every metric is authored at every examined time point (interval shared 0-120). The measurements capture the constraint's lifecycle: early coordination dominance, middle extractiveness accumulation (the technical debt), late suppression lock-in (reform becomes unthinkable).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (religious scholars, state cultural authority) should compute the constraint as rope or even mountain — a natural, necessary feature of Islamic civilization. The payer seats (commercial classes, technical military) should compute it as snare — an enforced extraction lacking genuine coordination justification. The administrative class sits between: they benefit from the literacy monopoly (power) but pay in administrative friction (constraint). The agenda-setter (state cultural authority) has arbitrage exit (can reform the script if it chooses) and thus experiences directionality near symmetric despite the enforcement role. The excluded reform officials have constrained exit (advocating reform carries institutional cost) and experience high directionality toward target, but are not named as victims because they do not directly bear extraction — their cost is suppressed voice, not material transfer. The engine should compute this divergence from the structural data: high d for commercial payers (trapped, identity_locked), low d for beneficiaries (identity_locked but collecting), moderate d for administrative class (powerful but constrained), zero d for excluded (not a seat in the arrangement).
 *
 * DIRECTIONALITY LOGIC:
 *   Ottoman religious scholars are low d (beneficiaries, institutional power, identity_locked) — the constraint flows wealth and authority to them through the monopoly it preserves. State cultural authority is moderate d (agenda_setter, institutional power, arbitrage exit) — they set and enforce the constraint but have the option to change it, making them symmetrically positioned despite their enforcement role. Ottoman administrative class is moderate-high d (powerful but constrained, biographical horizon, constrained exit) — they retain privilege through literacy monopoly but face rising friction from technical demands that this constraint blocks. Commercial literacy seekers are high d (moderate power, constrained exit, biographical horizon) — they pay a real cognitive and economic cost with limited alternatives. Technical military personnel are high d (organized power, constrained exit) — they absorb operational inefficiency from script isolation. Reform-minded officials are excluded rather than targeted because they are not positioned as payers — their cost is suppressed voice, not material transfer, and no structural data describes what flows from them through the constraint. The reading's own logic: continuity framing treats script preservation as a public good (coordination function), so the extraction of cognitive cost from technical classes should not register as an intentional transfer. But the metrics record it as extraction because the cognitive cost is real and the coordination function is separable from the script monopoly. This gap between the reading's self-description (coordination) and the structural fact (enforced access denial) is exactly the material the engine's per-seat divergence computation should expose.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading assigns the founding problem (preserving Islamic institutional continuity in a multiethnic empire) as contested status — religious traditionalists say it remains live (Islam and Ottoman identity are inseparable), while reform-minded officials and technical classes say it is solved (Islamic textual tradition survives any script change; technical literacy is now the binding constraint). The disappearance verdict is world_rearranges — the constraint's removal would reorganize the Ottoman administrative and commercial structure. These two together (founding_problem_status=contested + disappearance_verdict=world_rearranges) are the mandatrophy mismatch: the state defends a constraint whose original justification is disputed, yet that constraint structures real dependencies. The measurement series shows extractiveness rising through t=80 then plateauing, while suppression rises and plateaus at t=120 — this is the mandatrophy accumulation pattern. The constraint's justification (Islamic continuity preservation) grows thinner as suppression grows thicker, because the suppression is what must maintain the constraint once its coordination function becomes visible as separable from its enforcement object (script monopoly). By t=100, the theater_ratio (0.42) shows less than half of the activity is the stated function (preserving Islamic continuity) and more than half is performative defense of the script standard itself. This is the signature of mandatrophy without declared sunset — a commitment made for a live reason, that reason becomes contested, but the enforcement intensifies instead of the commitment sunsetting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Is script unity structurally necessary for Ottoman administrative coordination, or is the coordination function (unified state documents) separable from the script monopoly (enforcement of Arabic over alternatives)?',
    'Historical comparison: examine periods where Ottoman administrative practice relaxed script boundaries (e.g., Turkish-language documents in later periods, minority scripts for local administration) and assess whether coordination function persisted.',
    'If coordination and monopoly are separable, the constraint is pure extraction riding on a real coordination function (tangled_rope). If inseparable, part of the measured extraction is the inherent cost of maintaining textual unity (genuine rope, not tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether script unity for coordination requires monopoly enforcement.').

omega_variable(
    islamic_continuity_contingency,
    'Is maintaining Arabic script genuinely necessary to preserve Islamic textual tradition and institutional continuity, or is the Islamic tradition robust enough to survive script change?',
    'Longitudinal study of post-script-change societies (Morocco, Indonesia, Turkey after 1928, Kazakhstan, Azerbaijan): measure continuity of Islamic scholarship, institutional authority, and theological practice before and after orthographic transition.',
    'If the tradition survives script change with continuity intact, the constraint''s claimed coordination function (preservation of Islamic institutional lineage) is decoupled from its actual enforcement object (script monopoly). This would support the modernization reading''s claim that script is a neutral technical tool.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(islamic_continuity_contingency, empirical, 'Whether Islamic textual tradition requires Arabic script for preservation.').

omega_variable(
    suppression_internalization_divergence,
    'Is the measured suppression (0.76) a structural barrier (educational monopoly, legal prohibition) or an internalized belief (the subject population believes script change is identity-threatening)?',
    'Post-constraint ethnography: in contexts where script reform occurs (Turkey, Kazakhstan, North Africa), measure resistance to the new script and adoption rates among populations that grew up under the old script. Compare adoption speed with other script systems (e.g., adoption of Latin script by non-European languages). If internalized suppression is present, populations carry the suppression past the structural barrier.',
    'If suppression is mostly structural, reform would face initial resistance then rapid adoption. If internalized, populations would resist even after structural barriers are removed, suggesting identity-fusion. This informs whether the constraint''s extraction is sustainable (identity-fused targets resist exit even after mechanism removal) or brittle (structural barrier only).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_divergence, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    reading_foreclosure_contingency,
    'Does the continuity reading logically foreclose the modernization reading, or do they coexist as live alternatives?',
    'Logical analysis: the continuity reading asserts ''Arabic script is necessary for Islamic institutional continuity.'' The modernization reading asserts ''Script is a neutral tool; institutional continuity survives any script.'' These are logically contradictory at the level of necessities (necessary vs. not necessary). However, they could coexist if the continuity reading is about legitimacy claims (the state claims script is necessary) while the modernization reading is about empirical fact (institutional continuity persists). If the readings are about different referents (state-claim vs. historical-fact), they coexist; if they are both about the same empirical claim, one forecloses the other.',
    'If forecloses: the adoption of one reading (say, by court decision or legislative mandate) logically requires the other reading''s abandonment within the same commitment framework. If coexists: different factions can hold different readings without logical contradiction, and the kernel''s future depends on political power, not logical resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_contingency, conceptual, 'Logical relationship between continuity and modernization readings.').

omega_variable(
    state_authority_capture_risk,
    'Is the state cultural authority genuinely an independent agenda-setter, or is it captured by religious institutional interests?',
    'Examine cases where state cultural authority makes decisions opposed to religious institution preferences (e.g., secular reforms, minority script protection, secular education mandates). If such decisions occur, the authority has independent directionality; if all decisions align with religious institution benefit, the authority is captured and should be reclassified.',
    'If captured: the state cultural authority should not be modeled as an independent agenda-setter; it becomes an extension of religious institution power. The directionality computation would shift — the single orchestrator becomes two institutional actors with aligned interests. If independent: the state cultural authority has genuine arbitrage exit (can choose to reform the script) even if it currently chooses not to.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_capture_risk, empirical, 'Whether state cultural authority is independent or captured by religious institution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__continuity_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__continuity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(orth_tr_t20, orthographic_kernel__continuity_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(orth_tr_t40, orthographic_kernel__continuity_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(orth_tr_t60, orthographic_kernel__continuity_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(orth_tr_t80, orthographic_kernel__continuity_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(orth_tr_t100, orthographic_kernel__continuity_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement(orth_tr_t120, orthographic_kernel__continuity_reading, theater_ratio, 120, 0.42).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__continuity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(orth_be_t20, orthographic_kernel__continuity_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(orth_be_t40, orthographic_kernel__continuity_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(orth_be_t60, orthographic_kernel__continuity_reading, base_extractiveness, 60, 0.67).
narrative_ontology:measurement(orth_be_t80, orthographic_kernel__continuity_reading, base_extractiveness, 80, 0.69).
narrative_ontology:measurement(orth_be_t100, orthographic_kernel__continuity_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(orth_be_t120, orthographic_kernel__continuity_reading, base_extractiveness, 120, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__continuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(orth_su_t20, orthographic_kernel__continuity_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(orth_su_t40, orthographic_kernel__continuity_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(orth_su_t60, orthographic_kernel__continuity_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(orth_su_t80, orthographic_kernel__continuity_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement(orth_su_t100, orthographic_kernel__continuity_reading, suppression_requirement, 100, 0.76).
narrative_ontology:measurement(orth_su_t120, orthographic_kernel__continuity_reading, suppression_requirement, 120, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__continuity_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__modernization_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__rupture_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, ottoman_commercial_isolation).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, military_technical_debt).

% DUAL FORMULATION NOTE:
% This constraint is part of the orthographic_kernel constraint family. The kernel is a single persisting commitment (the state's script policy and its legitimacy claims) that different readings instantiate as different constraints. The continuity_reading (this story) frames script preservation as coordination for Islamic institutional continuity, assigning high extractiveness to the payer seats (technical/commercial classes blocked from script flexibility). The modernization_reading frames script as a neutral technical tool, assigning high extractiveness to the beneficiary seats (religious institutions defending a script monopoly against evidence of techonological harm). The rupture_reading frames script change as identity construction, assigning low extractiveness to the state (change is freedom, not extraction) and high extractiveness to the continuity defenders (clinging to false natural law). Each reading has a different epsilon because each attributes different causal structure to the same institutional fact. The three stories are linked by network.affects_constraints to enable contention analysis: the engine can compute how adoption of one reading's classification constrains the viability of the other readings' classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel__continuity_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
