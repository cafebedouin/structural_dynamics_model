% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Equality Clause Scope — Restrictive Originalist Reading (Founding-Class Settlement)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the equality_clause_scope kernel:
 *   the restrictive originalist reading, under which the equality guarantee
 *   binds with the scope the founding generation's social contract gave it —
 *   propertied white males as political actors — and every wider claim to
 *   equal standing must purchase entry through formal amendment. The reading
 *   supplies a genuine coordination function (one determinate referent, one
 *   legitimate change mechanism) wrapped around a monopoly on equal standing
 *   held by a narrow class, which is why the claimed type is tangled_rope
 *   rather than rope or snare. Per the claim/metric independence rule, the
 *   claimed type and the metric values below are authored independently: the
 *   metrics describe the arrangement's actual operation across the interval,
 *   and the engine computes per-seat classifications from the structural
 *   data. The epsilon value is reading-indexed over the fixed referent (the
 *   standing narrow-scope arrangement), per OQ-26: this reading grants the
 *   settlement's legitimacy, so its epsilon sits well below what a
 *   universalist seat would author over the same referent, but the asymmetry
 *   it concedes — monopolized standing, supermajority-priced inclusion —
 *   keeps it well above zero even by the reading's own account of the
 *   mechanism.
 *
 * KEY AGENTS:
 *   - - propertied_white_male_political_class: Primary beneficiary (powerful/arbitrage) — holds the equal standing the scope confers and authors the rules that confer it
 *   - - originalist_judicial_apparatus: Agenda setter (institutional/constrained) — administers the reading and collects interpretive authority from maintaining it
 *   - - enslaved_and_free_black_persons: Heaviest-cost target (powerless/trapped) — bore enslavement and total exclusion, bought every gain by amendment
 *   - - women_denied_political_standing: Target (powerless/trapped) — coverture and franchise exclusion until the Nineteenth Amendment
 *   - - unproperted_white_men: Secondary target (moderate/constrained) — property qualifications until Jacksonian-era entry
 *   - - expansion_claimants_outside_scope: Excluded voice (organized/constrained) — locked out of the interpretive conversation, routed to Article V
 *   - - constitutional_historians: Analytical observer (analytical/analytical) — owns the archival record every reading argues from
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.34).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.38).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.34).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Equality Clause Scope — Restrictive Originalist Reading (Founding-Class Settlement)").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '4897e261-135d-415a-9497-d410eb544b02').
narrative_ontology:cs_kernel_codification('4897e261-135d-415a-9497-d410eb544b02', fixed_text).
narrative_ontology:cs_authority_grounding('4897e261-135d-415a-9497-d410eb544b02', lineage).
narrative_ontology:cs_interpretation_layer_present('4897e261-135d-415a-9497-d410eb544b02').
narrative_ontology:cs_reading_relation('4897e261-135d-415a-9497-d410eb544b02', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('4897e261-135d-415a-9497-d410eb544b02', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('4897e261-135d-415a-9497-d410eb544b02', foundational, equal_standing_fixed_by_founding_consent).
narrative_ontology:cs_axiom_status(equal_standing_fixed_by_founding_consent, holdable).
narrative_ontology:cs_axiom_grounding('4897e261-135d-415a-9497-d410eb544b02', equal_standing_fixed_by_founding_consent, conventional).
narrative_ontology:cs_axiom('4897e261-135d-415a-9497-d410eb544b02', foundational, article_v_sole_legitimate_expansion_path).
narrative_ontology:cs_axiom_status(article_v_sole_legitimate_expansion_path, holdable).
narrative_ontology:cs_axiom_grounding('4897e261-135d-415a-9497-d410eb544b02', article_v_sole_legitimate_expansion_path, conventional).
narrative_ontology:cs_reference_frame('4897e261-135d-415a-9497-d410eb544b02', founding_era_social_contract_settlement).
narrative_ontology:cs_drift_state('4897e261-135d-415a-9497-d410eb544b02', contemporary_post_amendment_expansion, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4897e261-135d-415a-9497-d410eb544b02', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_male_political_class).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_and_free_black_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women_denied_political_standing).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, unproperted_white_men).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, consent_based_legitimacy).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, article_v_amendment_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the franchise, eligibility for office, and full protection of the equality guarantee as the founding compact defined its parties. Their political standing is the arrangement's product: they vote, sue, and govern as equals while everyone else must purchase entry through amendment. They face no pressure to leave an arrangement they authored and continue to staff.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_male_political_class, beneficiary,
    powerful, generational, arbitrage, national).

% Federal courts and the originalist legal movement administer the reading: they decide which equality claims state a constitutional violation and which must seek relief elsewhere, while their scholarly arm supplies the historical method that legitimizes the line-drawing. The arrangement returns interpretive authority to them — the power to say what the text settled — while exposing them to legitimacy criticism whenever the line excludes popular majorities.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_judicial_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% At the founding, most were held in slavery the compact expressly protected; free Black persons were barred from franchise, office, and equal protection in most states. Every gain — emancipation, citizenship, ballot access, equal protection — arrived only through amendment won at extraordinary cost, after the arrangement's beneficiaries had exhausted violent defense of the old scope.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_and_free_black_persons, payer,
    powerless, generational, trapped, national).

% Excluded from franchise, office, jury service, and much of contract and property law under coverture, with the equality guarantee read not to reach them. Political standing arrived only with the Nineteenth Amendment, and equal-citizenship claims still route through provisions the narrow reading does not itself supply.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women_denied_political_standing, payer,
    powerless, generational, trapped, national).

% White men without property were told the compact rested on consent yet were denied the vote by property qualifications in most states into the 1820s and 1840s. They forced entry through state reform movements and episodes like the Dorr Rebellion — the cheapest inclusion the arrangement ever granted, and one the reading's own logic counts as amendment-level change.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, unproperted_white_men, payer,
    moderate, biographical, constrained, national).

% Groups and claimants whose equality arguments fall outside the founding scope; they are not parties to the interpretive settlement that defines their standing. Their route in runs through Article V: supermajorities in two-thirds of Congress and three-quarters of the states, assembled with the consent of the very class the expansion would dilute.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansion_claimants_outside_scope, excluded,
    organized, generational, constrained, national).

% Scholars of the founding, Reconstruction, and rights expansion who document what the founding generation understood, what the amendments changed, and what the interpretive coalitions claim. They hold no vote in the arrangement and take no side in the contest, but their archival record is the evidentiary terrain every reading fights on.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, propertied_white_male_political_class).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single determinate referent for the equality guarantee — the founding generation's public meaning — so interpreters, litigants, and officials coordinate on one knowable standard, and routes all change through one legitimate mechanism (Article V) instead of case-by-case judicial revision.
% TRANSFER_FUNCTION: Moves equal political standing, franchise access, and rights-protection from every person outside the founding class to propertied white males; moves the cost of inclusion onto outsiders as supermajority amendment campaigns; and moves interpretive authority to the judiciary that administers the line.
% ABSENT_VOICES: The excluded themselves: enslaved and free Black persons, women, and the unpropertied were absent from the conversation that fixed the scope and remain outside the interpretive coalition that maintains it. Their standing objection — that a consent-based compact cannot bind those it never admitted — is heard only when they assemble amendment-level majorities, that is, after the arrangement has priced their entry.
% DISAPPEARANCE_RATIONALE: If the scope-fixing reading vanished overnight, every equality claim currently routed to amendment would re-enter ordinary interpretation; the founding-descended political class would lose the interpretive moat around its standing; courts would need a new account of why some claims fail; and the amendment-threshold pricing of inclusion — the arrangement's core transfer mechanism — would collapse.
% FOUNDING_PROBLEM: Securing a stable republic among fractious states and factions required a fixed, consent-based compact; the equality declaration was drafted inside a society structured by property, race, and gender hierarchy, and its scope was set by who counted as parties to the compact.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary class: ratification-era records assembled by constitutional historians (state convention debates, the three-fifths and fugitive-slave clauses) attest the founding scope; Reconstruction congressional debates and civil-rights historiography attest that expansion came by amendment against the old scope's armed defenders. No source outside the beneficiary class attests that the narrow scope remains normatively binding today — the reading's contemporary defenders argue fidelity and stability, not ongoing need for the exclusion itself.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).
:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Endpoint extractiveness is 0.34: the amendments the reading itself accepts have hollowed the narrow scope's operative content, leaving residual extraction in the pricing of further inclusion and the narrowing of unenumerated-rights claims. Suppression (0.38, raw and unscaled) reflects today's doctrinal barriers and the Article V threshold; the series shows it was far higher when the scope required slave codes, coverture, poll taxes, and terror to hold. Theater_ratio crosses 0.5 at the endpoint: as the substantive scope became indefensible, maintenance shifted toward performative fidelity — method legitimizing outcomes more than constraining them — which is Goodhart-drift territory. Accessibility_collapse is 0.50 because alternatives persist: rival readings remain live and the amendment route stays open, so this is nowhere near natural-law collapse. Resistance is 0.78 — abolitionism, suffrage organizing, the Civil War, the civil-rights movement, and continuing interpretive contest; few constructed constraints have met heavier resistance. The measurement series run on one shared seven-point grid so every metric is authored at every examined time point. The series is not cyclical: the t=80 to t=120 extractiveness rebound is the Redemption/Jim Crow enforcement wave re-tightening a scope the Reconstruction Amendments had loosened — a documented enforcement reversal, not an oscillation mechanism. Coalition note: each trapped victim class lacked standalone leverage; the amendments came only when cross-class coalitions (abolition, suffrage, labor) assembled supermajorities — the arrangement's pricing mechanism worked exactly as designed, which is itself diagnostic.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute differently. From the propertied class's position the arrangement is the settlement their ancestors consented to and the stability they fund; from the trapped payer seats the same structure operated as enforced exclusion whose every exit was priced at supermajority cost. The originalist judicial seat occupies a third position: it does not hold the standing monopoly itself, but its authority is paid out of maintaining the line, so it computes nearer the beneficiary end than its non-participation in the victim rolls would suggest. The engine derives this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The propertied class is the structural beneficiary (holds the conferred standing, faces no exit pressure — d near the beneficiary end). Enslaved and free Black persons, women, and the unproperted are the targets (bore the transfer, trapped or constrained exit — d near the target end, amplified by powerlessness and trap). Expansion claimants sit at high d with slightly better position: organized, so their eventual coalition capacity is real, but their entry price is set by the beneficiaries themselves. The judiciary is the one seat the automatic derivation would misplace: it appears in no beneficiary or victim list, so structural derivation would land it near symmetric, but it demonstrably collects interpretive authority from administering the reading — hence the directionality override setting the institutional atom to d=0.25. Only one stakeholder holds the institutional atom, so the override touches no other seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting the founding class's standing inside a consent compact — is dead as an open justification; nobody outside the beneficiary class defends the exclusion itself anymore. What survives is partial and real: a fixed referent and a single legitimate change mechanism still solve a live coordination problem for the polity at large. The classification guards against both failure modes: calling the constraint a pure snare ignores the genuine coordination function (predictable meaning, legitimate change path) that even opponents rely on; calling it a rope ignores that the coordination was built around a standing monopoly the beneficiaries actively defended with violence and still defend interpretively. The rising theater ratio marks the drift risk: if the method-substance decoupling completes (see omega method_substance_decoupling), what remains is theatrical maintenance of a mandate whose substance was amended away — the piton signature — and the mismatch consumer should watch founding_problem_status x disappearance_verdict accordingly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the restrictive_originalist reading of kernel equality_clause_scope; what would each sibling reading change structurally if adopted?',
    'Comparative classification across the three sibling stories: expansive_universalist widens the beneficiary set to all persons and dissolves the amendment-threshold pricing mechanism; progressive_textualist keeps the amendment path but relocates the principle into the text, lowering the legitimacy threshold for expansion without judicial universalism.',
    'The beneficiary set, the victim set, and the effective extraction all move together with the reading choice; cross-reading epsilon divergence over the identical referent is the measured quantity, not noise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame indexicality: one kernel, three readings, three distinct constraints.').

omega_variable(
    tacit_consent_binding_force,
    'Does the social-contract frame bind persons who never consented — and can descendants be bound by a compact that excluded their ancestors from the consenting class?',
    'Political-philosophy analysis of express versus tacit consent doctrine, tested against the historical record of who was actually admitted to the founding compact and on what terms.',
    'If tacit consent fails for the excluded, the reading''s legitimacy foundation collapses, epsilon rises sharply toward the universalist seat''s value, and the constraint drifts from tangled_rope toward snare — enforcement without a consent basis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_consent_binding_force, conceptual, 'Whether the consent theory that grounds the reading can reach the people it excludes.').

omega_variable(
    method_substance_decoupling,
    'Is contemporary originalism maintaining the narrow-scope substance or only the method, given that the substance was formally amended away?',
    'Code originalist outcomes in equality-relevant cases against counterfactual textualist and pragmatic-method outcomes; if outcome distributions converge across methods, the surviving constraint is method-performance, not scope-enforcement.',
    'If method-only, the constraint is closer to theatrical maintenance of a dead mandate (piton-side drift) and the theater_ratio trajectory is the leading indicator; if substance survives in doctrine, extraction continues at the measured rate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(method_substance_decoupling, empirical, 'Whether the reading''s live content is the historical scope or merely the interpretive method.').

omega_variable(
    stability_function_genuineness,
    'Does fixed-meaning coordination deliver genuine rule-of-law stability, or does the stability claim function as cover for interest protection by the beneficiary class?',
    'Measure outcome variance and reversal rates under originalist-dominant versus pragmatist-dominant interpretive eras; genuine coordination predicts lower variance independent of which coalition holds power.',
    'If the stability claim is cover, the coordination gate weakens and the constraint''s classification slides toward pure extraction; if genuine, the tangled_rope claim holds and part of the measured cost is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_function_genuineness, empirical, 'Whether the constraint''s coordination function is real or a cover story for the standing monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 0, 236).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__restrictive_originalist, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__restrictive_originalist, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(equa_tr_t40, observed).
narrative_ontology:measurement(equa_tr_t80, equality_clause_scope__restrictive_originalist, theater_ratio, 80, 0.24).
narrative_ontology:measurement_basis(equa_tr_t80, observed).
narrative_ontology:measurement(equa_tr_t120, equality_clause_scope__restrictive_originalist, theater_ratio, 120, 0.34).
narrative_ontology:measurement_basis(equa_tr_t120, observed).
narrative_ontology:measurement(equa_tr_t160, equality_clause_scope__restrictive_originalist, theater_ratio, 160, 0.4).
narrative_ontology:measurement_basis(equa_tr_t160, observed).
narrative_ontology:measurement(equa_tr_t200, equality_clause_scope__restrictive_originalist, theater_ratio, 200, 0.47).
narrative_ontology:measurement_basis(equa_tr_t200, observed).
narrative_ontology:measurement(equa_tr_t236, equality_clause_scope__restrictive_originalist, theater_ratio, 236, 0.52).
narrative_ontology:measurement_basis(equa_tr_t236, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__restrictive_originalist, base_extractiveness, 0, 0.88).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__restrictive_originalist, base_extractiveness, 40, 0.84).
narrative_ontology:measurement_basis(equa_be_t40, observed).
narrative_ontology:measurement(equa_be_t80, equality_clause_scope__restrictive_originalist, base_extractiveness, 80, 0.56).
narrative_ontology:measurement_basis(equa_be_t80, observed).
narrative_ontology:measurement(equa_be_t120, equality_clause_scope__restrictive_originalist, base_extractiveness, 120, 0.67).
narrative_ontology:measurement_basis(equa_be_t120, observed).
narrative_ontology:measurement(equa_be_t160, equality_clause_scope__restrictive_originalist, base_extractiveness, 160, 0.6).
narrative_ontology:measurement_basis(equa_be_t160, observed).
narrative_ontology:measurement(equa_be_t200, equality_clause_scope__restrictive_originalist, base_extractiveness, 200, 0.41).
narrative_ontology:measurement_basis(equa_be_t200, observed).
narrative_ontology:measurement(equa_be_t236, equality_clause_scope__restrictive_originalist, base_extractiveness, 236, 0.34).
narrative_ontology:measurement_basis(equa_be_t236, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__restrictive_originalist, suppression_requirement, 0, 0.74).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__restrictive_originalist, suppression_requirement, 40, 0.8).
narrative_ontology:measurement_basis(equa_su_t40, observed).
narrative_ontology:measurement(equa_su_t80, equality_clause_scope__restrictive_originalist, suppression_requirement, 80, 0.83).
narrative_ontology:measurement_basis(equa_su_t80, observed).
narrative_ontology:measurement(equa_su_t120, equality_clause_scope__restrictive_originalist, suppression_requirement, 120, 0.77).
narrative_ontology:measurement_basis(equa_su_t120, observed).
narrative_ontology:measurement(equa_su_t160, equality_clause_scope__restrictive_originalist, suppression_requirement, 160, 0.64).
narrative_ontology:measurement_basis(equa_su_t160, observed).
narrative_ontology:measurement(equa_su_t200, equality_clause_scope__restrictive_originalist, suppression_requirement, 200, 0.48).
narrative_ontology:measurement_basis(equa_su_t200, observed).
narrative_ontology:measurement(equa_su_t236, equality_clause_scope__restrictive_originalist, suppression_requirement, 236, 0.38).
narrative_ontology:measurement_basis(equa_su_t236, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the equality principle' covers three structurally distinct claims that cannot share one story, because each assigns a different beneficiary set, a different victim set, and a different epsilon over the same referent. The restrictive_originalist story (this file) is upstream in legitimacy rhetoric — its fidelity claim is cited by defenders against both siblings — while the expansive_universalist and progressive_textualist stories carry the expansion-side structure. All three files link one another through network.affects_constraints; contamination or credibility shifts in any member propagate to the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__restrictive_originalist, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
