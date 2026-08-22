% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__ecumenical_reunion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__ecumenical_reunion_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: creed_381_pneumatology__ecumenical_reunion_reading
 *   human_readable: Ecumenical Reunion Reading: Bilateral Recognition of Filioque/Mono-Procession Pluralism
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   This story authors the ecumenical-reunion reading of the creed_381
 *   pneumatology kernel: the arrangement, formalized in outline by documents
 *   like the 1995 Pontifical Council for Promoting Christian Unity
 *   clarification and mirrored in subsequent Orthodox-Catholic joint
 *   statements, under which Filioque and mono-procession are treated as
 *   co-legitimate regional theological expressions within a single (or
 *   approaching) communion, replacing the historical pattern of unilateral
 *   creedal amendment with bilateral recognition. This reading does not
 *   adjudicate whether Filioque is theologically correct or whether the 381
 *   creed is inviolable — it authors the coordination framework that lets
 *   both sides defer that question while proceeding with partial reunion.
 *   Sibling constraints (filioque_reading, monoprocession_reading) author the
 *   two substantive doctrinal claims this scaffold declines to resolve; each
 *   carries its own ε and its own victim/beneficiary structure, per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - ecumenical_dialogue_commissions: agenda_setter (institutional/constrained) — drafts and administers the bilateral framework
 *   - eastern_western_reunion_advocates: beneficiary (organized/mobile) — institutional relevance tied to the reunion project
 *   - diaspora_mixed_communion_parishes: beneficiary/payer (moderate/constrained) — practical sacramental access, minor catechetical cost
 *   - monoprocession_traditionalists: excluded (organized/trapped) — object that pluralism legitimizes an alleged breach
 *   - filioque_magisterial_authorities: excluded (institutional/constrained) — object that magisterial clarification is demoted to regional custom
 *   - historical_theologians: observer (analytical/analytical) — assess whether this is genuine transition or indefinite holding pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__ecumenical_reunion_reading, 0.28).
domain_priors:suppression_score(creed_381_pneumatology__ecumenical_reunion_reading, 0.18).
domain_priors:theater_ratio(creed_381_pneumatology__ecumenical_reunion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__ecumenical_reunion_reading, scaffold).
narrative_ontology:human_readable(creed_381_pneumatology__ecumenical_reunion_reading, "Ecumenical Reunion Reading: Bilateral Recognition of Filioque/Mono-Procession Pluralism").
narrative_ontology:topic_domain(creed_381_pneumatology__ecumenical_reunion_reading, "historical_theology/ecclesiastical_authority").

narrative_ontology:has_sunset_clause(creed_381_pneumatology__ecumenical_reunion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__ecumenical_reunion_reading, '5e5ea9f7-0927-47b1-a939-e18b8b566e8f').
narrative_ontology:cs_kernel_codification('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', fixed_text).
narrative_ontology:cs_authority_grounding('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', distributed).
narrative_ontology:cs_reading_relation('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', creed_381_pneumatology__filioque_reading, influences).
narrative_ontology:cs_reading_relation('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', creed_381_pneumatology__monoprocession_reading, influences).
narrative_ontology:cs_axiom('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', foundational, theological_pluralism_compatible_with_single_communion).
narrative_ontology:cs_axiom_status(theological_pluralism_compatible_with_single_communion, holdable).
narrative_ontology:cs_axiom_grounding('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', theological_pluralism_compatible_with_single_communion, conventional).
narrative_ontology:cs_axiom('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', foundational, bilateral_recognition_supersedes_unilateral_creedal_amendment).
narrative_ontology:cs_axiom_status(bilateral_recognition_supersedes_unilateral_creedal_amendment, holdable).
narrative_ontology:cs_axiom_grounding('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', bilateral_recognition_supersedes_unilateral_creedal_amendment, instrumental).
narrative_ontology:cs_reference_frame('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', pre_1054_undivided_communion).
narrative_ontology:cs_drift_state('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', post_1995_vatican_clarification_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5e5ea9f7-0927-47b1-a939-e18b8b566e8f', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__ecumenical_reunion_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, ecumenical_dialogue_commissions).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, eastern_western_reunion_advocates).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, diaspora_mixed_communion_parishes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(creed_381_pneumatology__ecumenical_reunion_reading, diaspora_mixed_communion_parishes).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__ecumenical_reunion_reading, theological_pluralism_within_single_communion).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__ecumenical_reunion_reading, bilateral_recognition_supersedes_unilateral_imposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and administer the bilateral framework documents (e.g. joint statements modeled on the 1995 Vatican clarification and Orthodox responses) that formally permit each communion to retain its own procession language while recognizing the other's expression as legitimate within a restored or approaching full communion. They set the terms of what counts as acceptable regional variance and could, in principle, revise or dissolve the arrangement if reunion is achieved or abandoned.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, ecumenical_dialogue_commissions, agenda_setter,
    institutional, generational, constrained, global).

% Clergy, theologians, and lay movements invested in restored communion. They gain standing, funding, and platform from the scaffold's existence — conferences, joint liturgies, academic appointments in comparative theology — and their institutional relevance depends on the reunion project remaining active rather than resolved.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, eastern_western_reunion_advocates, beneficiary,
    organized, generational, mobile, continental).

% Congregations in mixed Eastern/Western population centers who benefit practically from being able to worship, intermarry, and receive sacraments across the procession divide without being forced to affirm the 'wrong' clause. They bear a lesser cost: theological ambiguity in catechesis and occasional confusion about which creed variant a given parish uses.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, diaspora_mixed_communion_parishes, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__ecumenical_reunion_reading, diaspora_mixed_communion_parishes, payer).

% Hold that the 381 creed is inviolable and that permitting Filioque as a co-equal regional expression, even under bilateral recognition, still legitimizes what they regard as a historically unilateral doctrinal amendment. They are present in the wider kernel dispute but are not the authors of, and largely do not consent to, this scaffold's framing; their objection is structural, not merely tactical.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, monoprocession_traditionalists, excluded,
    organized, civilizational, trapped, continental).

% Hold that conciliar/papal magisterium possesses standing authority to clarify Trinitarian doctrine, of which Filioque is an instance; the ecumenical reunion reading's bilateral-recognition frame implicitly brackets that authority claim as one 'regional expression' among others rather than as binding clarification, which this reading's own advocates experience as a demotion they did not request.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, filioque_magisterial_authorities, excluded,
    institutional, civilizational, constrained, global).

% Study the ninth-through-twenty-first-century procession dispute, the 1995 Vatican clarification, and Orthodox conciliar responses as data. They can assess whether the scaffold framework functions as genuine transitional coordination toward reunion or as an indefinitely extended holding pattern.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working framework under which two communions with historically incompatible procession formulas can share sacraments, joint governance structures, and mutual recognition of clergy without either side being required to renounce its formula first — solving the genuine coordination problem of how partial reunion can proceed before full doctrinal convergence is reached.
% TRANSFER_FUNCTION: Moves institutional legitimacy and ecumenical initiative toward the commissions and advocacy networks that administer the bilateral-recognition apparatus, and moves practical sacramental access toward mixed-communion laity; it does not transfer material resources from an identifiable victim class, which is why no victims are declared.
% ABSENT_VOICES: Monoprocession traditionalists and Filioque-magisterial-authority advocates are each, from their own side, structurally uncomfortable with being folded into a 'both acceptable as regional expression' frame — the traditionalists because it treats an alleged unilateral breach as a legitimate variant, the magisterial-authority side because it treats a doctrinal clarification as a mere regional custom. Neither authored this reading's terms.
% DISAPPEARANCE_RATIONALE: If the bilateral-recognition scaffold vanished, the ecumenical commissions and reunion-advocacy networks would lose their operating framework and much of their institutional purpose (world_rearranges from their seat); but from the monoprocession-traditionalist and Filioque-magisterial seats, the underlying doctrinal dispute predates the scaffold and would simply resume its pre-scaffold adversarial posture — for them the scaffold's disappearance changes little of substance (world_unchanged from their seat). The verdict is genuinely contested across seats rather than resolvable to one answer.
% FOUNDING_PROBLEM: The 1054 mutual excommunications and the centuries-long procession dispute left Eastern and Western communions structurally divided with no working mechanism for partial reunion; the scaffold was built to permit incremental sacramental and institutional reunion without requiring either side to resolve the underlying Trinitarian dispute first.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of the 1995 Vatican clarification and subsequent Orthodox-Catholic joint commission statements (e.g. the North American Orthodox-Catholic Theological Consultation) attest that formal reunion remains unachieved and that mixed-communion pastoral accommodation is the primary operative use of the framework, corroborating the founding problem's continued liveness from outside the advocacy networks that administer it.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__ecumenical_reunion_reading, contested).
narrative_ontology:founding_problem_status(creed_381_pneumatology__ecumenical_reunion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__ecumenical_reunion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(creed_381_pneumatology__ecumenical_reunion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__ecumenical_reunion_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__ecumenical_reunion_reading_tests).
:- end_tests(creed_381_pneumatology__ecumenical_reunion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-moderate (0.28 by 2025) because the scaffold's operation does not transfer material resources from an identifiable victim class — no group's assets or labor are extracted through this arrangement's mechanism, which is why victims[] is empty. Suppression is low (0.18) because participation is voluntary at the parish and commission level; no coercive apparatus compels adherence to bilateral recognition. Theater ratio is authored as rising over the interval (0.25 to 0.40) because thirty years of joint commissions, consultations, and statements have produced substantial declarative and ceremonial activity relative to actual structural reunion achieved — the scaffold shows early signs of Goodhart drift, where dialogue-commission activity becomes a proxy for reunion progress rather than a means to it. Accessibility collapse is moderate (0.35): once the bilateral framework is understood, walking away from it doesn't collapse alternatives the way a natural law would, but institutional momentum makes exit from the ecumenical apparatus costly for the advocacy networks built around it. Resistance is moderate-high (0.55), reflecting genuine pushback from both excluded seats.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting commissions' seat, this looks like patient, principled scaffolding toward eventual reunion — exactly the coordination function a scaffold is supposed to serve. From the excluded traditionalist and magisterial seats, the same arrangement looks like a permanent postponement dressed as transition: thirty years in, the sunset condition (full doctrinal reunion) has not arrived, and the theater ratio's rise suggests the scaffold may be drifting toward self-perpetuation. The engine's per-seat computation should register this divergence structurally rather than resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecumenical dialogue commissions sit near the beneficiary end: they administer the framework and derive institutional purpose and resources from its continuation. Reunion advocates and mixed-communion parishes are declared beneficiaries with genuine coordination gains (sacramental access, institutional standing) and low displaced cost. No group is declared a victim because the scaffold's mechanism does not extract resources from an identifiable population — its costs are distributed as ambiguity and institutional friction rather than concentrated transfer. The excluded seats (monoprocession_traditionalists, filioque_magisterial_authorities) are not victims of extraction; they are objectors to the framing itself, which is why they carry role excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The declared has_sunset_clause: true is doing real work here: a scaffold whose sunset condition (achieved doctrinal reunion or formal schism resolution) has not fired after thirty years is exactly the case the mandatrophy check exists to flag. The founding_problem_status is authored live rather than dead, corroborated by outside historians, which keeps this from being a straightforward zombie-scaffold case — the underlying division genuinely persists. But the rising theater_ratio is the leading indicator that this scaffold could still degrade toward the piton condition (persistent, mostly performative, no party positioned to fix it) if the sunset condition never fires and dialogue-commission activity continues to substitute for actual reunion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reunion_reading_kernel_disambiguation,
    'Is the ecumenical-reunion reading a genuine third position on the pneumatology kernel, or is it structurally parasitic on the other two readings — i.e., does ''bilateral recognition'' actually resolve anything, or does it merely relabel an unresolved dispute as resolved for institutional convenience?',
    'Track whether any concrete reunion milestone (full sacramental intercommunion, joint conciliar governance) is reached under the bilateral-recognition framework within a defined follow-up window (e.g. 2025-2050); absence of such a milestone after an extended period would support the parasitic reading.',
    'If parasitic, this reading''s claimed_type may drift from scaffold toward piton (persistent, no party positioned to fix it, largely performative) rather than remaining a genuine transitional coordination mechanism; if genuine, the scaffold classification and low ε remain structurally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reunion_reading_kernel_disambiguation, conceptual, 'Whether the reunion reading is a real third position or an institutionally convenient deferral.').

omega_variable(
    sibling_reading_foreclosure_structure,
    'Does adopting the bilateral-recognition frame structurally weaken the monoprocession_reading''s inviolability claim by normalizing Filioque as a co-equal regional variant, even though this reading does not explicitly foreclose that sibling?',
    'Analyze whether Orthodox communities that formally endorse bilateral recognition subsequently see erosion in catechetical emphasis on mono-procession exclusivity, versus communities that reject the reunion frame and maintain it undiminished.',
    'If erosion is observed, the influences relation declared toward monoprocession_reading understates the downstream pressure; the reunion reading''s neutrality claim would be partly cosmetic rather than structurally even-handed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_structure, empirical, 'Whether declared neutrality toward siblings masks asymmetric downstream pressure on one of them.').

omega_variable(
    coordination_vs_deferral_ambiguity,
    'Is the low-moderate authored extractiveness correct, or does the scaffold quietly extract theological clarity from the laity (accepting permanent ambiguity as the price of institutional peace) in a way that should register as a diffuse, hard-to-name victim class?',
    'Survey mixed-communion parish catechesis to determine whether laity experience the ambiguity as a benefit (freedom from doctrinal conflict) or a cost (inability to receive clear formation); this would surface a victim class currently unauthored.',
    'If laity experience net cost, victims[] should be non-empty and the claimed_type reassessed toward tangled_rope; as currently authored (empty victims, scaffold), the reading treats the arrangement as unambiguously coordination-positive for those inside it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_deferral_ambiguity, empirical, 'Whether diffuse catechetical ambiguity constitutes an unauthored victim class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__ecumenical_reunion_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t1995, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(cree_tr_t2001, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2001, 0.29).
narrative_ontology:measurement(cree_tr_t2007, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2007, 0.32).
narrative_ontology:measurement(cree_tr_t2013, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2013, 0.35).
narrative_ontology:measurement(cree_tr_t2019, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(cree_tr_t2025, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(cree_be_t1995, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement(cree_be_t2001, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2001, 0.2).
narrative_ontology:measurement(cree_be_t2007, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2007, 0.22).
narrative_ontology:measurement(cree_be_t2013, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2013, 0.24).
narrative_ontology:measurement(cree_be_t2019, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2019, 0.26).
narrative_ontology:measurement(cree_be_t2025, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(creed_381_pneumatology__ecumenical_reunion_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(creed_381_pneumatology__ecumenical_reunion_reading, filioque_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__ecumenical_reunion_reading, monoprocession_reading).

% DUAL FORMULATION NOTE:
% This story is the third member of the creed_381_pneumatology constraint family. filioque_reading and monoprocession_reading each author a substantive doctrinal/authority claim with sharply differing ε and victim structures (the former claims magisterial authority to clarify Trinitarian doctrine; the latter claims the 381 creed's inviolability and treats unilateral amendment as breach). This reading (ecumenical_reunion_reading) authors the meta-level coordination claim that both can be held as regional expressions within single communion. It is linked to both siblings via affects_constraints because bilateral recognition changes the legitimacy conditions and institutional resource availability for each substantive claim without logically foreclosing either — see cs_structure.reading_relations for the typed edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
