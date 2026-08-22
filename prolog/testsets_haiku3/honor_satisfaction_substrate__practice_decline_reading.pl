% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Code Normative Substrate Under Legal Suppression
 *   domain: cultural/legal/historical
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'honor_satisfaction_substrate' — specifically, the
 *   practice_decline_reading. It asserts that the honor code persisted as
 *   normative substrate (its core claims about masculine status, reputation,
 *   public performance remained valid and binding) while dueling declined due
 *   to exogenous enforcement (legal prohibition, institutional barriers,
 *   opportunity cost of criminal prosecution and social destruction). The
 *   constraint is NOT a mountain eroding naturally; it is a rope
 *   (coordination mechanism) under active suppression. Under this reading,
 *   the code survives and adapts: formal letters replace duels, institutional
 *   reputation gates replace public combat, military and regional honor codes
 *   persist in attenuated forms. The alternative readings
 *   (cultural_contraction_reading, composite_overdetermined_reading) propose
 *   that the honor code itself underwent foundational transformation or that
 *   decline was overdetermined by multiple non-independent causal pathways.
 *   This reading claims: the code endured; the practice changed because it
 *   became impractical, not because it became unthinkable.
 *
 * KEY AGENTS:
 *   - honor_code_practitioners (powerful, identity-locked): aristocratic/professional men who maintain status through reputation
 *   - men_of_honor_trapped_in_code (moderate, identity-locked): men experiencing the code as inescapable constraint despite legal suppression of dueling
 *   - institutional_reputation_gatekeepers (institutional, mobile): churches, universities, military academies enforcing honor norms
 *   - legal_enforcement_apparatus (institutional, mobile): state systems criminalizing dueling
 *   - excluded_lower_status_men (powerless, constrained): barred from participation in the code
 *   - women_as_honor_objects (powerless, trapped): whose status is managed by men through the code but who cannot participate in it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.31).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.72).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Code Normative Substrate Under Legal Suppression").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "cultural/legal/historical").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '104f5e1c-b5d6-47fd-8387-5a695072b7b6').
narrative_ontology:cs_kernel_codification('104f5e1c-b5d6-47fd-8387-5a695072b7b6', distributed).
narrative_ontology:cs_authority_grounding('104f5e1c-b5d6-47fd-8387-5a695072b7b6', lineage).
narrative_ontology:cs_interpretation_layer_present('104f5e1c-b5d6-47fd-8387-5a695072b7b6').
narrative_ontology:cs_reading_relation('104f5e1c-b5d6-47fd-8387-5a695072b7b6', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('104f5e1c-b5d6-47fd-8387-5a695072b7b6', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('104f5e1c-b5d6-47fd-8387-5a695072b7b6', foundational, honor_code_normative_persistence).
narrative_ontology:cs_axiom_status(honor_code_normative_persistence, holdable).
narrative_ontology:cs_axiom_grounding('104f5e1c-b5d6-47fd-8387-5a695072b7b6', honor_code_normative_persistence, conventional).
narrative_ontology:cs_axiom('104f5e1c-b5d6-47fd-8387-5a695072b7b6', foundational, dueling_decline_exogenous_suppression).
narrative_ontology:cs_axiom_status(dueling_decline_exogenous_suppression, holdable).
narrative_ontology:cs_axiom_grounding('104f5e1c-b5d6-47fd-8387-5a695072b7b6', dueling_decline_exogenous_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('104f5e1c-b5d6-47fd-8387-5a695072b7b6', aristocratic_reputation_satisfaction_framework).
narrative_ontology:cs_drift_state('104f5e1c-b5d6-47fd-8387-5a695072b7b6', late_nineteenth_century_criminalization_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('104f5e1c-b5d6-47fd-8387-5a695072b7b6', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_code_practitioners).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, institutional_reputation_gatekeepers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, men_of_honor_trapped_in_code).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, honor_code_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aristocratic and upper-middle-class men who stake their social standing, marriage eligibility, business trust, and community position on reputation for honorable conduct. The honor code provides a stable frame for status competition and a mechanism to recover reputation after insult. They defend the code as civilizing force even as dueling becomes legally prohibited; they transition to non-violent honorable conduct (formal letters, public rebuttals, social sanctions) while maintaining the underlying framework. Their identity as 'a man of honor' is constituted through adherence to the code; exit means social death.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_code_practitioners, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, honor_code_practitioners, payer).

% Men socialized into the honor code who experience it as an inescapable constraint on action and public performance. They cannot refuse a challenge without bearing the weight of cowardice and social exclusion; they cannot admit to any insult without requiring public satisfaction; they are trapped between the code's demands and legal prohibition on dueling. The suppression is internalized — the code operates through reputation threat and identity fusion, not external force. Even as dueling becomes illegal, the requirement to respond to insult persists as normative pressure.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, men_of_honor_trapped_in_code, payer,
    moderate, biographical, identity_locked, national).

% Churches, universities, military academies, professional associations, and civic organizations that validate and enforce honor norms through membership, ordination, commission, and social standing. They set what conduct counts as honorable, adjudicate reputation disputes, and deploy exclusion as enforcement. They maintain the honor code as a normative substrate even after dueling becomes illegal by reframing its core claims (masculine integrity, public reputation, status through conduct) into non-violent forms. They benefit from the code's legitimizing function for institutional hierarchy.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, institutional_reputation_gatekeepers, agenda_setter,
    institutional, generational, mobile, national).

% State legal systems that criminalize dueling and prosecute challenges, seconds, and participants. They provide the exogenous suppression that makes dueling prohibitively costly — not by delegitimizing the honor code itself, but by making its practice outcome illegal, dangerous, and socially catastrophic. They do not argue the code is false; they simply remove the practical ability to satisfy it through combat.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, legal_enforcement_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Working-class and poor men who are structurally barred from full participation in the honor code because they lack the property, education, and social standing required for challenges to be recognized or defended. They develop parallel codes (street honor, craftsman reputation) but are not parties to the aristocratic/professional honor system. They are excluded from the coordination problem the upper-class honor code solves.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, excluded_lower_status_men, excluded,
    powerless, biographical, constrained, national).

% Women whose sexual reputation, family honor, and social standing are managed through the honor code of male relatives and husbands, but who cannot themselves be agents in the code (cannot challenge, cannot duel, cannot restore their own reputation through honorable combat). Their honor is derivative and subject to male representatives' actions. They are explicitly excluded from participation in the coordination mechanism, though their status is what much of the code is defending.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, women_as_honor_objects, excluded,
    powerless, biographical, trapped, national).

% Scholars and analysts studying the persistence and transformation of honor codes across societies. They observe that dueling declined despite the code remaining normatively active, suggesting the constraint is not a mountain (inevitable) but a rope (maintained coordination) now under pressure from exogenous enforcement.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__practice_decline_reading, institutional_reputation_gatekeepers).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__practice_decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code coordinates reputation management: it establishes a stable frame for status competition among elites, provides a mechanism to recover reputation after public insult (through challenge and satisfaction), and creates mutual enforcement of truthfulness and reliability in reputation claims. Without it, reputation disputes would be anarchic; with it, social standing is knowable and contestable through formal procedures.
% TRANSFER_FUNCTION: Moves social standing, marriageability, and business trust from those perceived as cowardly, dishonest, or weak to those perceived as brave, truthful, and honorable. The code transfers autonomy and agency from women and lower-status men (who cannot participate) to upper-status men (who control the honor frame). It transfers the risk of violent death to men who refuse challenges or cannot satisfy the code through non-violent means.
% ABSENT_VOICES: Working-class men and all women are structurally excluded: they would attest that the honor code is not a natural coordination solution but an arrangement that concentrates status and agency among privileged males and imposes costs on those it classifies as without honor or as objects of male honor. Lower-status honor cultures (street honor, artisan reputation) would argue the frame is not universal but particularist to aristocratic and professional classes.
% DISAPPEARANCE_RATIONALE: If the honor code disappeared, status competition would reorganize (merit, wealth, institutional credentials become more salient), challenges and duels would cease within years, marriage markets would decouple from honor reputation, and the social position of women and lower-status men would shift. Institutional hierarchies would need alternative legitimation. The code is not background infrastructure; it is a socially constructed frame that shapes who has standing to compete for what.
% FOUNDING_PROBLEM: Aristocratic and professional elites faced a coordination problem: how to maintain reliable reputation and status hierarchy in the absence of centralized formal record-keeping or institutional credentialing; how to settle disputes about who wronged whom without dissolving into cycles of revenge; how to make truthfulness in reputation claims enforceable when lying about one's honor could change one's social position.
% FOUNDING_PROBLEM_CORROBORATION: Honor-culture advocates (historians studying pre-modern aristocracy, contemporary defenders of military and regional honor traditions) attest the problem was live and the code provided genuine coordination. Historical sociologists and anthropologists (Wyatt-Brown on Southern honor, Cohen on honor cultures, Gamble on dueling decline) attest the founding problem was real but argue the code's effectiveness degraded as legal prohibition and institutional transformation made dueling costly, forcing the code to adapt or lose its coordinating function. Legal historians and early-modern scholars attest exogenous enforcement (criminalization) was decisive in dueling's decline. No voices outside the benefiting classes corroborate the code as a necessary coordination solution; excluded groups attest it served exclusion more than coordination.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.31 at t=1900) because the honor code does solve a genuine coordination problem: how to manage status and reputation publicly, how to settle disputes without violence, how to make truthfulness enforceable. The coordination function persists across the interval — extractiveness rises slightly from t=1600 to t=1750 as the code becomes more formalized, then stabilizes as legal pressure mounts. Suppression is HIGH (0.72 at t=1900) because the constraint's persistence depends not on participant preference but on active legal enforcement (criminalization of dueling), institutional exclusion of alternatives, and the identity-locked bind that makes exit unthinkable for practitioners. Theater is moderate (0.28 at t=1900) because a growing fraction of honorable action becomes performative as dueling becomes illegal: formal letters replacing combat, social rebuttals replacing public satisfaction, military codes formalizing what was once organic practice. Accessibility collapse is moderate (0.68) because alternatives exist (not honoring insults, joining lower-status honor cultures, migrating to regions where dueling is still practiced) but are structurally costly or identity-foreclosing. Resistance is moderate (0.54) because some men resist legal suppression and continue dueling despite prohibition, institutional reform movements challenge the code's legitimacy, and excluded groups resist being outside it. The coercion grid shows that individual-level suppression rises dramatically (0.12 to 0.68) as legal apparatus activates, while structural suppression rises more modestly (0.08 to 0.62) because the code remains culturally salient. Class-level resistance rises (0.38 to 0.58) as lower-status men develop parallel honor codes and reject aristocratic framing. The measurements track a constraint under sustained external pressure: the code does not erode naturally, but it does become increasingly performative and enforced rather than organic.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (institutional reputation gatekeepers, legal enforcement apparatus) should compute very differently from the trapped seats (men_of_honor_trapped_in_code). For the gatekeepers, this is genuine coordination they maintain; they see the code as civilizing, reputation-stabilizing, and worth defending or reforming. For the trapped practitioners, the code operates as inescapable normative pressure: they cannot refuse a challenge without social death, cannot admit to insult, cannot exit the frame. From the legal apparatus seat, dueling is disorder to be suppressed; the code itself is viewed as a cover story or obstacle to clear justice. From the trapped practitioner seat, legality is irrelevant — the code's demand persists regardless of law. The engine computes these divergences from the structural data: identity-locked exit amplifies extraction for trapped practitioners; institutional power and mobile exit reduce extraction for agenda-setters.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda-setters (institutional gatekeepers, legal apparatus) benefit from the code or from its enforcement: they validate reputation, maintain hierarchy, or enforce the law. They have mobile exit (they can change what the code requires, can relocate enforcement to other behaviors) and institutional power. Their directionality is near beneficiary (d ≈ 0.2–0.3). Practitioners benefit from having a stable reputation frame but pay through identity-lock: they cannot exit without social death. Their directionality is near symmetric to slightly target (d ≈ 0.45–0.60). Trapped practitioners explicitly bear the cost: they are the direct targets of suppression and identity-lock, constrained to respond to insult despite legal jeopardy. Their directionality is firmly target (d ≈ 0.75–0.85). Excluded groups (lower-status men, women) are barred from the coordination benefit entirely, so they experience only costs (their status is subject to male honor decisions, they cannot participate or defend themselves). Their directionality is hard target, but they are outside the primary constraint — they participate in parallel or derivative honor systems.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT claim mandatrophy. The founding problem (reliable reputation management among elites) is CONTESTED but still live for practitioners and institutional gatekeepers; the code adapts (formal letters replace duels) rather than persisting as theater. If the founding problem were dead (if reliable reputation management were now solved by other institutional means: newspapers, credit reports, institutional credentials, legal contract enforcement) and the code persisted as pure theater, mandatrophy would apply — we would see pure suppression with no coordination benefit. Instead, this reading tracks a rope under pressure: the coordination function remains; the primary practice (dueling) was made impractical; the code survives in adapted forms. The terminal state this reading predicts is NOT a zombie constraint but a transformed but genuine one — military honor codes, regional honor cultures, professional codes of conduct all carry forward the core claims about reputation and trustworthiness without the lethal practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    code_persistence_vs_cultural_transformation,
    'Did the honor code persist as a live normative system after dueling declined, or did it undergo foundational transformation into a dignity-based system where it became vestigial?',
    'Textual and ethnographic analysis of 19th and 20th century honor codes (military, Southern, professional): do they explicitly continue core claims about public reputation satisfaction and honorable combat, or do they reformulate core claims around internal moral worth and dignity independent of public verdict?',
    'If the code persisted with core claims intact (this reading''s assertion), the constraint is rope-under-pressure with a surviving coordination function. If the code transformed its core claims, the constraint would be piton or snare (the founding problem is dead but institutional machinery persists). This is the most direct test of reading divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(code_persistence_vs_cultural_transformation, empirical, 'Whether the honor code persisted or underwent foundational delegitimation').

omega_variable(
    exogenous_vs_endogenous_causation,
    'What proportion of dueling''s decline is attributable to exogenous legal/institutional suppression (this reading''s claim) versus endogenous cultural delegitimation (cultural_contraction_reading''s claim)?',
    'Causal analysis of dueling decline across jurisdictions: did decline correlate primarily with criminalization timing, or with cultural transformation timing, or both? Natural experiments from regions where legal suppression occurred without cultural transformation (or vice versa) would disambiguate.',
    'If legal suppression is the primary driver (high correlation with criminalization, continued practice where legal enforcement was weak), practice_decline_reading is supported and the constraint is rope-under-pressure. If cultural transformation is primary (practice declined even before criminalization, or persisted after criminalization where culture remained permissive), cultural_contraction_reading is supported. If both are substantial and causally entangled, composite_overdetermined_reading is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_causation, empirical, 'Causal decomposition of exogenous suppression versus endogenous transformation').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression primarily structural (legal penalties, institutional exclusion, economic destruction) or internalized (the code''s own demands have become self-suppressing through shame and identity fusion)?',
    'Post-suppression trajectories: if suppression were purely structural, men who migrated to regions without legal prohibition should resume dueling; if suppression is internalized, they would continue the adapted forms. Ethnographic interviews and historical records of emigration behavior would test this.',
    'If structural, the constraint is correctly classified as rope-under-external-pressure; exit options for identity-locked practitioners would improve if legal suppression were removed. If internalized, the constraint is more accurately snare — the suppression is carried by the code''s adherents themselves, not imposed externally. This affects exit_options coding and directionality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural (externally imposed) or internalized (carried by adherents)').

omega_variable(
    reading_frame_alternative_cultural_contraction,
    'This reading frames the honor code as a persisting normative system under legal pressure. The cultural_contraction_reading frames it as a foundationally transformed system where honor became dignity and the code became vestigial. Could both frames be locally valid — one capturing practitioners'' self-understanding, the other capturing historical observers'' external analysis?',
    'Ethnographic comparison: do contemporary honor-culture practitioners (military, regional Southern) describe their codes as continuous with historical dueling-era honor, or as a transformation into dignity-based systems? How do they position themselves relative to historical dueling?',
    'If practitioners claim continuity and historical observers see transformation, the frames capture different seats'' lived experience. This would NOT refute either reading but would suggest the need for both to model the perceptual divergence. It would increase the strength of the composite_overdetermined_reading''s claim that multiple non-commensurable processes are in play.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_frame_alternative_cultural_contraction, conceptual, 'Alternative framing: are readings describing the same process from different epistemic seats, or different processes?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1600, tn=1900
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(class), 1600, 0.72).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(class), 1900, 0.58).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(individual), 1600, 0.85).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(individual), 1900, 0.62).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(organizational), 1600, 0.78).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(organizational), 1900, 0.7).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(structural), 1600, 0.65).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(structural), 1900, 0.7).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_substrate__practice_decline_reading, resistance(class), 1600, 0.38).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_substrate__practice_decline_reading, resistance(class), 1900, 0.58).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_substrate__practice_decline_reading, resistance(individual), 1600, 0.32).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_substrate__practice_decline_reading, resistance(individual), 1900, 0.48).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_substrate__practice_decline_reading, resistance(organizational), 1600, 0.28).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_substrate__practice_decline_reading, resistance(organizational), 1900, 0.52).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_substrate__practice_decline_reading, resistance(structural), 1600, 0.22).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_substrate__practice_decline_reading, resistance(structural), 1900, 0.42).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(class), 1600, 0.42).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(class), 1900, 0.62).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(individual), 1600, 0.45).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(individual), 1900, 0.68).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(organizational), 1600, 0.38).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(organizational), 1900, 0.55).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(structural), 1600, 0.35).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(structural), 1900, 0.5).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_substrate__practice_decline_reading, suppression(class), 1600, 0.18).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_substrate__practice_decline_reading, suppression(class), 1900, 0.72).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_substrate__practice_decline_reading, suppression(individual), 1600, 0.12).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_substrate__practice_decline_reading, suppression(individual), 1900, 0.68).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_substrate__practice_decline_reading, suppression(organizational), 1600, 0.1).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_substrate__practice_decline_reading, suppression(organizational), 1900, 0.75).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_substrate__practice_decline_reading, suppression(structural), 1600, 0.08).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_substrate__practice_decline_reading, suppression(structural), 1900, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__practice_decline_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'honor_satisfaction_substrate'. It asserts that the honor code's core claims about masculine status and reputation satisfaction persisted as normative substrate, while dueling declined due to exogenous legal/institutional suppression. The cultural_contraction_reading proposes the code underwent foundational transformation; the composite_overdetermined_reading proposes both suppression and transformation operated with non-independent causal pathways. Each reading instantiates a different constraint with different ε, different beneficiary structure, and different terminal type. All three readings are linked via network.affects_constraints to enable comparative analysis of reading divergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__practice_decline_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
