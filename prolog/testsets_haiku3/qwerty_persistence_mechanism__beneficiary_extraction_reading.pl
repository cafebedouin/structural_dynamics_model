% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Persistence via Incumbent Protection (Beneficiary-Extraction Reading)
 *   domain: economic_history/technology/path_dependence
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'qwerty_persistence_mechanism.' The beneficiary-extraction reading holds
 *   that QWERTY persists not because it is technically superior, nor through
 *   passive coordination lock-in, but because incumbent typewriter
 *   manufacturers (Remington, Union Typewriter), established typing schools,
 *   and already-trained typists actively maintain the standard to protect
 *   their training investments and market position. They suppress
 *   alternatives through standardization monopolies, gatekeeping of
 *   professional certification, control of equipment manufacturing, and
 *   narrative framing of QWERTY as 'natural' or 'inevitable.' The reading
 *   identifies specific beneficiaries (incumbent manufacturers, typing
 *   schools, existing typists) and specific victims (alternative-layout
 *   designers, workers forced into suboptimal switching costs). This reading
 *   coexists with a lock-in reading (path-dependent coordination failure
 *   despite inferiority) and a naturalization reading (QWERTY persists
 *   because it became adequate and alternatives lapsed through fair
 *   competition). All three readings share the same referent—the persistence
 *   of QWERTY into the digital era—but differ in their account of the
 *   mechanism sustaining it. The three readings are sibling constraints,
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - incumbent_typewriter_manufacturers (Remington, Union Typewriter, et al.) — institutional power, arbitrage exit, global scope; set standardization policy and enforce it through manufacturing/distribution control.
 *   - established_typing_schools — organized power, constrained exit, national scope; aligned with incumbents, gatekeep professional certification and training curricula.
 *   - existing_typists — moderate power, identity-locked exit, global scope; benefit from QWERTY lock-in, resist alternative-layout adoption, defend their training investment.
 *   - alternative_keyboard_designers (Dvorak et al.) — moderate power, constrained exit, global scope; bear the cost of suppressed alternatives, face market rejection despite technical superiority.
 *   - potential_adopters — powerless, trapped exit, global scope; face mandatory QWERTY training, no meaningful choice point, absorb efficiency costs without awareness of suppression.
 *   - academic researchers — analytical seat; document the active maintenance mechanisms and the contrast with naturalization framings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.71).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence via Incumbent Protection (Beneficiary-Extraction Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '8840592f-c8f2-447c-afba-85ee1eecd45f').
narrative_ontology:cs_kernel_codification('8840592f-c8f2-447c-afba-85ee1eecd45f', distributed).
narrative_ontology:cs_authority_grounding('8840592f-c8f2-447c-afba-85ee1eecd45f', extraction).
narrative_ontology:cs_interpretation_layer_present('8840592f-c8f2-447c-afba-85ee1eecd45f').
narrative_ontology:cs_reading_relation('8840592f-c8f2-447c-afba-85ee1eecd45f', qwerty_persistence_mechanism__lock_in_reading, influences).
narrative_ontology:cs_reading_relation('8840592f-c8f2-447c-afba-85ee1eecd45f', qwerty_persistence_mechanism__naturalization_reading, influences).
narrative_ontology:cs_axiom('8840592f-c8f2-447c-afba-85ee1eecd45f', foundational, institutional_suppression_is_operative).
narrative_ontology:cs_axiom_status(institutional_suppression_is_operative, holdable).
narrative_ontology:cs_axiom_grounding('8840592f-c8f2-447c-afba-85ee1eecd45f', institutional_suppression_is_operative, empirically_contingent).
narrative_ontology:cs_axiom('8840592f-c8f2-447c-afba-85ee1eecd45f', foundational, beneficiary_preservation_requires_active_gatekeeping).
narrative_ontology:cs_axiom_status(beneficiary_preservation_requires_active_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('8840592f-c8f2-447c-afba-85ee1eecd45f', beneficiary_preservation_requires_active_gatekeeping, instrumental).
narrative_ontology:cs_reference_frame('8840592f-c8f2-447c-afba-85ee1eecd45f', qwerty_as_incumbents_competitive_advantage).
narrative_ontology:cs_drift_state('8840592f-c8f2-447c-afba-85ee1eecd45f', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8840592f-c8f2-447c-afba-85ee1eecd45f', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, established_typing_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, existing_typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_designers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, potential_adopters_of_superior_layouts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Remington, Union Typewriter, and other dominant manufacturers controlled manufacturing standards, retail distribution, and keyboard specifications. They actively standardized QWERTY across their product lines and lobbied standardization bodies, framing QWERTY as the 'professional standard' to lock users into their ecosystem. Benefited from artificial switching costs that prevented users from adopting superior layouts, forcing competitors to adopt QWERTY to remain compatible.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Typing schools had invested in QWERTY-specific curricula, instructor training, and practice materials. They actively promoted QWERTY as the 'standard' for commercial typists, creating network effects that reinforced the layout. They resisted alternative layout advocates and discouraged student exploration of superior alternatives. Their certification and training authority created switching costs for workers seeking to adopt new layouts.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, established_typing_schools, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, established_typing_schools, agenda_setter).

% Typists already trained on QWERTY benefited from the lock-in: their training remained valuable, competition from workers with superior-layout training was suppressed, and their skills commanded market premium as the only recognized professional standard. Their identity as 'professional typists' was fused with QWERTY competence, making switching unthinkable despite personal awareness that alternatives existed.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, existing_typists, beneficiary,
    moderate, biographical, identity_locked, global).

% Inventors and proponents of superior layouts (Dvorak, et al.) faced active suppression: manufacturers refused to license or produce their designs, typing schools refused to teach them, and users faced re-training costs and professional marginalization for adopting them. The standardization monopoly prevented market testing of alternatives. Their designs remained technically superior but commercially dead.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_designers, payer,
    moderate, biographical, constrained, global).

% Users entering the typing profession had no meaningful choice: all training, all employment, all equipment standardized on QWERTY. The 'standard' was presented as natural/inevitable rather than a choice point. Switching costs for anyone reaching professional competence were prohibitive (retraining, certification loss, job market penalties). They bore the cost of suboptimal efficiency without awareness that the constraint was maintained by beneficiary action.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, potential_adopters_of_superior_layouts, payer,
    powerless, biographical, trapped, global).

% Smaller typewriter manufacturers and new entrants could not profitably produce alternative layouts because no users would adopt them (trained only on QWERTY), but were forced to adopt QWERTY themselves to remain compatible with the ecosystem. They were excluded from any meaningful choice in the standard-setting process, and the dominant manufacturers' control of retail channels made even QWERTY-compatible alternatives economically marginal.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_manufacturers, excluded,
    moderate, generational, trapped, national).

% Historians, economists, and ergonomics researchers document the transition from QWERTY's introduction (as one of many early options) to its monopoly status, mapping the active maintenance mechanisms: standardization campaigns, typing-school certification capture, manufacturer licensing restrictions, and pedagogical gatekeeping. They distinguish this reading from naturalization and lock-in framings.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, academic_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, uniform keyboard layout across all professional typing equipment enabled standardized training, interchangeable workers, and compatible equipment markets—solving a genuine coordination problem in the professionalization of typing as a specialized skill.
% TRANSFER_FUNCTION: Moves competitive advantage (market share, training monopoly, certification authority, wage premiums for typists) from alternative-layout proponents and new entrants to incumbent manufacturers, typing schools, and already-trained typists. The transfer is enforced by controlling standardization bodies, equipment manufacturing, and professional credentialing.
% ABSENT_VOICES: Alternative-layout designers, potential adopters who might have chosen superior layouts absent the switching costs, workers in non-English-speaking regions forced into QWERTY designs for their languages, ergonomists advocating layout changes for injury prevention. These voices were structurally excluded from standardization decisions, which were controlled by incumbent manufacturers and their aligned institutions.
% DISAPPEARANCE_RATIONALE: If the QWERTY standardization regime and its enforcement mechanisms disappeared, users could immediately adopt superior layouts (Dvorak, Colemak), manufacturers could differentiate on keyboard design, typing schools would compete on training the most efficient layout, and professionals would optimize for ergonomics rather than path-dependent training. The constraint's removal would restructure the equipment market and labor market for typing within years.
% FOUNDING_PROBLEM: Early typewriter era (1870s–1890s) had competing keyboard layouts with no clear winner; manufacturers and users needed a common standard to enable interoperable equipment and professional credentialing of typists.
% FOUNDING_PROBLEM_CORROBORATION: Technology historians and economic historians (David, Arthur, Liebowitz & Margolis) document that QWERTY's technical advantages were minimal; it was merely one acceptable option among several. The founding coordination problem was solved by the 1920s when QWERTY became dominant. Contemporary alternatives (Dvorak, introduced 1936) are measurably superior on ergonomic metrics, but the constraint persists through manufacturer/school gatekeeping, not because the founding problem remains live. Alternative-layout advocates and ergonomics researchers attest the founding problem is resolved but the constraint is maintained.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint transfers competitive advantage, market share, and certification authority from alternative designs to incumbents through artificial switching costs. The extraction is not 'natural' (a raw coordination solution) but maintained by specific actors' choices: manufacturers could license alternatives, typing schools could teach them, workers could adopt them—all are prevented by enforcement rather than by technical or physical barriers. Suppression (0.71) tracks the intensity of active gatekeeping: standardization campaigns, typing-school monopolies, manufacturer licensing restrictions, and pedagogical suppression of alternatives. Theater (0.42 at interval end) indicates that by 1990, a substantial portion of the maintenance activity is devoted to narrative justification ('professional standard,' 'industry best practice') rather than genuine functional necessity—the founding coordination problem was solved by the 1920s, yet the constraint persisted through performance and gatekeeping. Accessibility collapse (0.64 at interval end) reflects that by the late 20th century, alternatives appear impossible to enter despite their technical superiority; users see only QWERTY as 'the standard.' Resistance (0.58 at interval end) shows that throughout the period, vocal advocates for alternatives (Dvorak proponents, ergonomics researchers) mounted real opposition, but were excluded from standardization forums and manufacturing decisions. The temporal series trace the constraint's evolution from minimal enforcement (1873) through active standardization campaigns (1900–1920) to mature gatekeeping (1945–1990), with theater ratio rising as the original coordination rationale faded but enforcement mechanisms persisted.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent-manufacturer seat, QWERTY is a coordination standard they stewarded, justified by professional training consistency and equipment compatibility—a genuine beneficiary-preserving arrangement. From the alternative-layout-designer seat, QWERTY is an imposed monopoly enforced through manufacturing control and professional gatekeeping, explicitly designed to suppress competing designs. From the powerless-adopter seat, QWERTY appears as a 'natural professional standard,' not as an engineered lock-in—the enforcement is invisible. The engine computes these seats differently because they have different power atoms, exit options, and structural relationships to the constraint. The beneficiary-extraction reading highlights the beneficiaries' active role in maintaining suppression; a lock-in reading would emphasize path-dependent coordination without intentional suppression; a naturalization reading would emphasize QWERTY's adequacy relative to realistic alternatives. All three readings acknowledge QWERTY's persistence; they differ in the mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent manufacturers are near the full-beneficiary end (d near 0.2): they control the standard, benefit from it directly, have arbitrage-grade exit (could adopt alternatives if profitable), and bear no extraction cost. Established typing schools are near symmetric-to-beneficiary (d near 0.3–0.4): they benefit from certification gatekeeping but face some reputational pressure if the standard is publicly seen as suboptimal. Existing typists are beneficiaries (d near 0.2–0.3): their training remains valuable because alternatives are suppressed; they could theoretically adopt alternatives but identity fusion with professional QWERTY competence makes exit psychologically costly. Alternative-layout designers are full targets (d near 0.9): they bear the extraction (excluded market access, suppressed designs) and have constrained exit (can only advocate, cannot access manufacturing/credentialing channels). Potential adopters are near full targets (d near 0.85): they pay the cost of suboptimal ergonomics without meaningful choice, trapped by the educational system's QWERTY standardization. Academic researchers sit at analytical (d = 0.5): they bear no cost, collect no benefit, document the structure. The coercion grid shows how suppression and stakes inflation concentrated highest at the organizational level (typing schools, manufacturers) and structural level (standardization bodies) rather than spreading uniformly—beneficiary actors controlled the points of decision.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating a single keyboard standard for professional typists) was genuinely live in 1873–1900 and was solved by the 1920s—QWERTY became the dominant standard, and Dvorak's introduction in 1936 found no market adoption despite measurable ergonomic superiority. By 1970–1990, the founding problem was dead: coordination was achieved, QWERTY was universal, and the constraint persisted through pure gatekeeping rather than solving any live coordination problem. The theater_ratio trajectory (0.0 → 0.42) documents this decay: early maintenance activity solved a real problem; later maintenance is theatrical (certifications, standardization committees, professional credentialing) that persist because beneficiaries profit from them. The mandatrophy signal is the mismatch between (disappearance_verdict=world_rearranges, founding_problem_status=dead): if the constraint disappeared, markets would reorganize quickly to superior layouts, yet the founding problem's solution is already achieved. The constraint persists as pure extraction, maintained by beneficiary gatekeeping, not by functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_intent_vs_structure,
    'Did incumbent manufacturers and typing schools consciously suppress alternatives as a strategy to extract rents, or did they follow path-dependent professional practices without explicit intent to exclude?',
    'Historical archival evidence of boardroom decisions, internal memos, industry correspondence; interviews with participants; comparison of QWERTY standardization campaigns to contemporaneous suppression of other technical alternatives in other industries.',
    'If intentional, the constraint is clearly tangled_rope (deliberate extraction via coordination cover). If structural-but-unintentional, the classification moves toward rope (coordination with incidental beneficiary advantage). If intentional, the beneficiary-extraction reading is vindicated; if structural, the lock-in reading gains weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intent_vs_structure, empirical, 'Whether beneficiary suppression of alternatives was deliberate strategy or emergent institutional behavior.').

omega_variable(
    dvorak_feasibility_counterfactual,
    'If Dvorak layouts had been manufactured and taught from 1936 onward (the same active support incumbents gave QWERTY), would they have achieved comparable market penetration?',
    'Historical reconstruction from patent records, manufacturing capacity analysis, adoption case studies where alternatives were backed (e.g., alphabetic keyboard systems in non-English markets). Experimental adoption trials in military/government contexts where Dvorak was marginally supported (US Navy trials). Simulation models of adoption dynamics under different standardization scenarios.',
    'If Dvorak would have succeeded with equivalent institutional backing, the constraint is primarily enforced extraction via suppression (beneficiary-extraction reading supported). If Dvorak failed despite backing, path-dependent coordination or technical adequacy of QWERTY becomes more plausible (lock-in or naturalization readings supported).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dvorak_feasibility_counterfactual, conceptual, 'Whether alternative layouts were suppressed or naturally selected against.').

omega_variable(
    identity_lock_mechanism_clarity,
    'For existing typists (role=beneficiary, exit=identity_locked), is the identity fusion with QWERTY competence a deliberate inculcation by typing schools and employers, or an emergent byproduct of professional specialization?',
    'Analysis of typing-school curricula and training materials; interviews with instructors and learners; comparison of identity fusion dynamics in QWERTY vs. post-QWERTY typing instruction (where alternatives are available); psychological research on professional identity formation.',
    'If deliberately inculcated by schools/employers, the suppression mechanism is active and intentional (beneficiary-extraction reading strengthened). If emergent, the constraint may be maintained more by participant psychology than by institutional enforcement, shifting toward lock-in dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_clarity, empirical, 'Whether typist identity-lock is deliberately constructed or naturally emergent.').

omega_variable(
    kernel_reading_boundary,
    'Does the existence of documented active suppression (licensing restrictions, typing-school gatekeeping) necessarily establish the beneficiary-extraction reading as distinct from the lock-in reading, or can both mechanisms operate simultaneously within a single constraint?',
    'Clarification of whether ''lock-in'' means ''coordination without suppression'' (forecloses beneficiary-extraction) or ''coordination failure despite suppression'' (coexists with beneficiary-extraction). Specification of what empirical evidence would settle which reading applies.',
    'If lock-in and beneficiary-extraction foreclose each other, the engine reclassifies based on the dominant mechanism. If they coexist, the constraint is a hybrid, and both readings remain live. This omega clarifies the kernel contest itself, not just this reading''s claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether beneficiary suppression and path-dependent coordination are mutually exclusive mechanisms or compatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 1873, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1873, 0.0).
narrative_ontology:measurement_basis(qwer_tr_t1873, projected).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t1900, observed).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1920, 0.22).
narrative_ontology:measurement_basis(qwer_tr_t1920, observed).
narrative_ontology:measurement(qwer_tr_t1945, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1945, 0.35).
narrative_ontology:measurement_basis(qwer_tr_t1945, observed).
narrative_ontology:measurement(qwer_tr_t1970, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement_basis(qwer_tr_t1970, observed).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 1990, 0.42).
narrative_ontology:measurement_basis(qwer_tr_t1990, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1873, 0.15).
narrative_ontology:measurement_basis(qwer_be_t1873, projected).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement_basis(qwer_be_t1900, observed).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement_basis(qwer_be_t1920, observed).
narrative_ontology:measurement(qwer_be_t1945, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1945, 0.61).
narrative_ontology:measurement_basis(qwer_be_t1945, observed).
narrative_ontology:measurement(qwer_be_t1970, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1970, 0.66).
narrative_ontology:measurement_basis(qwer_be_t1970, observed).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement_basis(qwer_be_t1990, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1873, 0.05).
narrative_ontology:measurement_basis(qwer_su_t1873, projected).
narrative_ontology:measurement(qwer_su_t1900, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1900, 0.28).
narrative_ontology:measurement_basis(qwer_su_t1900, observed).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1920, 0.45).
narrative_ontology:measurement_basis(qwer_su_t1920, observed).
narrative_ontology:measurement(qwer_su_t1945, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1945, 0.58).
narrative_ontology:measurement_basis(qwer_su_t1945, observed).
narrative_ontology:measurement(qwer_su_t1970, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement_basis(qwer_su_t1970, observed).
narrative_ontology:measurement(qwer_su_t1990, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 1990, 0.71).
narrative_ontology:measurement_basis(qwer_su_t1990, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1873, tn=1990
narrative_ontology:measurement(qwer_grid_01, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(class), 1873, 0.05).
narrative_ontology:measurement(qwer_grid_02, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(class), 1990, 0.68).
narrative_ontology:measurement(qwer_grid_03, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(individual), 1873, 0.1).
narrative_ontology:measurement(qwer_grid_04, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(individual), 1990, 0.55).
narrative_ontology:measurement(qwer_grid_05, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(organizational), 1873, 0.12).
narrative_ontology:measurement(qwer_grid_06, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(organizational), 1990, 0.78).
narrative_ontology:measurement(qwer_grid_07, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(structural), 1873, 0.08).
narrative_ontology:measurement(qwer_grid_08, qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse(structural), 1990, 0.72).
narrative_ontology:measurement(qwer_grid_09, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(class), 1873, 0.62).
narrative_ontology:measurement(qwer_grid_10, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(class), 1990, 0.35).
narrative_ontology:measurement(qwer_grid_11, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(individual), 1873, 0.55).
narrative_ontology:measurement(qwer_grid_12, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(individual), 1990, 0.28).
narrative_ontology:measurement(qwer_grid_13, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(organizational), 1873, 0.58).
narrative_ontology:measurement(qwer_grid_14, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(organizational), 1990, 0.18).
narrative_ontology:measurement(qwer_grid_15, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(structural), 1873, 0.65).
narrative_ontology:measurement(qwer_grid_16, qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance(structural), 1990, 0.25).
narrative_ontology:measurement(qwer_grid_17, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(class), 1873, 0.08).
narrative_ontology:measurement(qwer_grid_18, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(class), 1990, 0.62).
narrative_ontology:measurement(qwer_grid_19, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(individual), 1873, 0.12).
narrative_ontology:measurement(qwer_grid_20, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(individual), 1990, 0.58).
narrative_ontology:measurement(qwer_grid_21, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(organizational), 1873, 0.15).
narrative_ontology:measurement(qwer_grid_22, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(organizational), 1990, 0.72).
narrative_ontology:measurement(qwer_grid_23, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(structural), 1873, 0.1).
narrative_ontology:measurement(qwer_grid_24, qwerty_persistence_mechanism__beneficiary_extraction_reading, stakes_inflation(structural), 1990, 0.65).
narrative_ontology:measurement(qwer_grid_25, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(class), 1873, 0.05).
narrative_ontology:measurement(qwer_grid_26, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(class), 1990, 0.71).
narrative_ontology:measurement(qwer_grid_27, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(individual), 1873, 0.03).
narrative_ontology:measurement(qwer_grid_28, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(individual), 1990, 0.62).
narrative_ontology:measurement(qwer_grid_29, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(organizational), 1873, 0.08).
narrative_ontology:measurement(qwer_grid_30, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(organizational), 1990, 0.75).
narrative_ontology:measurement(qwer_grid_31, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(structural), 1873, 0.02).
narrative_ontology:measurement(qwer_grid_32, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression(structural), 1990, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.18).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% QWERTY persistence is a contested kernel instantiated across three readings: beneficiary-extraction (active suppression by incumbents), lock-in (path-dependent coordination failure), and naturalization (QWERTY adequacy + fair competition). This story is the beneficiary-extraction reading, which identifies specific institutional beneficiaries and documents active suppression mechanisms. The sibling readings are separate constraint stories sharing the same referent (QWERTY's persistence) but different causal mechanisms. The three readings coexist in academic and policy discourse; neither logically forecloses the other, but the beneficiary-extraction reading influences the others by establishing that institutional suppression was possible and documented. Each reading carries its own ε (extractiveness), beneficiary/victim structure, and omegas addressing the reading-specific uncertainties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
