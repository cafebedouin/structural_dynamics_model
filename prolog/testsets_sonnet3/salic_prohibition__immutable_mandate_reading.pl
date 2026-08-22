% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law as Irrevocable Divine Mandate Excluding Female Succession
 *   domain: constitutional/dynastic/political
 *
 * SUMMARY:
 *   This story instantiates the immutable-mandate reading of the Salic
 *   prohibition kernel: the claim that agnatic-only succession is not merely
 *   a positive legal rule adopted by a particular dynasty but a fundamental,
 *   irrevocable law of the realm — antecedent to and binding upon any
 *   sovereign, discovered rather than enacted, and grounded in natural or
 *   divine order. Under this reading, any female or cognatic claim is not a
 *   weaker legal position to be weighed but a categorical nullity, which is
 *   precisely what licenses preventive war (e.g., the English claim
 *   triggering the Hundred Years' War, or later Spanish and Austrian
 *   succession conflicts) as legitimate enforcement rather than aggression.
 *   This is a distinct constraint from the sovereign-override reading (which
 *   would treat the rule as ordinary statute a king could revise) and from
 *   the cognatic-reversion reading (which denies the rule ever validly bound
 *   non-Frankish territories at all) — the three readings share a kernel text
 *   and history but diverge sharply on revisability, territorial scope, and
 *   whose claims are cognizable, and so are authored as three separate
 *   constraint stories per the eps-invariance principle.
 *
 * KEY AGENTS:
 *   - agnatic_male_claimants: Primary beneficiary (powerful/arbitrage) — inherits by categorical rule
 *   - agnatic_line_nobility: Agenda-setter (institutional/arbitrage) — codifies and administers exclusion
 *   - female_heirs_and_their_issue: Primary target (powerless/trapped) — categorically barred
 *   - cognatic_claimant_territories: Secondary target (moderate/constrained) — external claims overridden
 *   - royal_jurists_and_clergy: Doctrine architects (institutional/identity_locked) — construct the natural-law framing
 *   - subjects_of_contested_successions: Diffuse payer (powerless/trapped) — bears costs of enforcement wars
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.71).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.8).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Irrevocable Divine Mandate Excluding Female Succession").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional/dynastic/political").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, '3e0dca09-9764-4a72-a611-70677cb5944d').
narrative_ontology:cs_kernel_codification('3e0dca09-9764-4a72-a611-70677cb5944d', formalized).
narrative_ontology:cs_authority_grounding('3e0dca09-9764-4a72-a611-70677cb5944d', lineage).
narrative_ontology:cs_interpretation_layer_present('3e0dca09-9764-4a72-a611-70677cb5944d').
narrative_ontology:cs_reading_relation('3e0dca09-9764-4a72-a611-70677cb5944d', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('3e0dca09-9764-4a72-a611-70677cb5944d', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('3e0dca09-9764-4a72-a611-70677cb5944d', foundational, agnatic_succession_antecedent_to_sovereign_will).
narrative_ontology:cs_axiom_status(agnatic_succession_antecedent_to_sovereign_will, holdable).
narrative_ontology:cs_axiom_grounding('3e0dca09-9764-4a72-a611-70677cb5944d', agnatic_succession_antecedent_to_sovereign_will, deontological).
narrative_ontology:cs_axiom('3e0dca09-9764-4a72-a611-70677cb5944d', foundational, exclusion_of_female_line_is_natural_not_enacted_order).
narrative_ontology:cs_axiom_status(exclusion_of_female_line_is_natural_not_enacted_order, holdable).
narrative_ontology:cs_axiom_grounding('3e0dca09-9764-4a72-a611-70677cb5944d', exclusion_of_female_line_is_natural_not_enacted_order, theological).
narrative_ontology:cs_reference_frame('3e0dca09-9764-4a72-a611-70677cb5944d', agnatic_priority_as_fundamental_constitutional_law).
narrative_ontology:cs_drift_state('3e0dca09-9764-4a72-a611-70677cb5944d', early_modern_succession_crises, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('3e0dca09-9764-4a72-a611-70677cb5944d', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_male_claimants).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_line_nobility).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, male_line_dynastic_houses).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs_and_their_issue).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_claimant_territories).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, subjects_of_contested_successions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, royal_jurists_and_clergy).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, fundamental_law_of_the_realm_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, agnatic_priority_as_natural_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stand to inherit the crown or apanages purely by virtue of unbroken male-line descent, regardless of proximity or merit relative to female-line rivals. Invoke the law as fundamental and unamendable whenever their claim is closest under agnatic reckoning. Can mobilize legal scholars, clergy, and armies to defend the exclusion.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_male_claimants, beneficiary,
    powerful, generational, arbitrage, national).

% Parlements, royal jurists, and peerage houses codify and re-assert the exclusion at each succession crisis, framing it as fundamental law antecedent to any king's will. They administer the doctrine, adjudicate contested successions in their own favor, and treat any attempt at revision as an attack on the constitutional order itself.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_line_nobility, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Entire cadet branches and allied houses gain long-run succession prospects because female lines and their offspring are permanently removed from the line of inheritance. They benefit structurally even when no individual member currently holds a claim.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, male_line_dynastic_houses, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Are barred from inheriting or transmitting the crown regardless of birth order, capability, or the express wishes of a dying monarch. Their children lose all claim through the maternal line no matter how proximate in blood. They cannot appeal outside the dynastic-legal apparatus that excludes them, because that apparatus itself proclaims the exclusion irrevocable and prior to positive law.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs_and_their_issue, payer,
    powerless, biographical, trapped, national).

% Neighboring or annexed territories with their own inheritance customs permitting female or cognatic succession find their local law overridden or delegitimized whenever their claimant's title intersects a Salic-governed throne. War, papal arbitration, or forced renunciation are their only recourses, and all are costly and uncertain.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_claimant_territories, payer,
    moderate, generational, constrained, continental).

% Ordinary subjects bear the wars, taxation, and instability that flow from succession disputes fought to enforce or resist the exclusion — most visibly in prolonged dynastic wars justified as vindicating agnatic priority. They have no voice in the doctrine's application and absorb its costs regardless of outcome.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, subjects_of_contested_successions, payer,
    powerless, biographical, trapped, national).

% Legal scholars and churchmen construct the theological and juridical apparatus declaring the exclusion part of the fundamental, unwritten constitution of the realm, prior to and unalterable by any king. Their institutional authority and professional identity are bound up in maintaining that the doctrine is discovered natural law, not enacted policy — abandoning that claim would dissolve their interpretive monopoly.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, royal_jurists_and_clergy, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, royal_jurists_and_clergy, beneficiary).

% Would press strong genealogical claims through female descent were the exclusion not treated as absolute; under this reading their claims are not merely weaker but categorically void, and asserting them is framed as illegitimate usurpation inviting preventive war rather than adjudication.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, rival_dynastic_claimants_via_female_line, excluded,
    powerful, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, agnatic_male_claimants).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, non-negotiable succession rule that forecloses genealogical litigation by removing an entire category of claimants from contention in advance, reducing the field of rival claims a monarch's death can generate.
% TRANSFER_FUNCTION: Moves inheritance rights, territorial title, and political power away from female heirs and cognatic lines and consolidates them permanently within agnatic male lines and the noble houses whose fortunes are tied to them; moves the cost of enforcing this consolidation (war, instability, forced renunciations) onto subjects and rival territories.
% ABSENT_VOICES: Female claimants and their issue have no forum within the dynastic-legal apparatus itself to contest the doctrine, since that apparatus's own premise is that the exclusion precedes and binds any sovereign or court that might hear them; cognatic-succession territories are heard only through the register of war or forced treaty, never as legal equals within the Salic framework.
% DISAPPEARANCE_RATIONALE: If the immutable-mandate reading collapsed, female and cognatic claimants currently barred would immediately re-enter contention for multiple thrones and territories, cadet agnatic houses would lose exclusive long-run succession prospects, and the jurists/clergy whose authority rests on declaring the rule natural and unalterable would lose their interpretive monopoly over succession law.
% FOUNDING_PROBLEM: Medieval Frankish successor kingdoms faced repeated succession crises and civil war when multiple lines (agnatic and cognatic) could plausibly claim a throne; a categorical rule promised to foreclose litigation before it started by removing an entire class of claimants.
% FOUNDING_PROBLEM_CORROBORATION: Agnatic claimants, allied nobility, and the royal jurists who administer the doctrine attest the rule remains a live and necessary safeguard against succession chaos. Independent legal historians, canon lawyers examining the (largely retrospective) textual basis for the 'fundamental law' claim, and the excluded cognatic claimants and their allied territories attest the doctrine was substantially a post-hoc juridical construction assembled to settle a 14th-century succession dispute in France, not a pre-existing natural or divine law — corroboration from outside the beneficiary set is thin and largely adversarial.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high-moderate (0.71 at interval end) because the arrangement transfers a durable, high-value good (dynastic succession rights) from a categorically defined class to another, and because — under this reading specifically — the transfer is total and non-negotiable rather than a matter of degree. Suppression is authored higher still (0.80) because the doctrine's persistence depends on actively foreclosing legal recourse for excluded claimants: the same apparatus that would hear an appeal is the one that proclaims the rule prior to its own jurisdiction. Theater ratio rises over the interval (0.20 to 0.42) as the doctrine's 'natural/divine law' framing increasingly functions as retrospective justification manufactured after the 1316-1328 French succession crises rather than a genuinely ancient constitutional discovery — juridical and clerical scholarship investing more into proving antiquity than into the substantive coordination function. Accessibility collapse is high (0.72) because once a claimant accepts the fundamental-law framing, no internal legal avenue remains open; resistance is correspondingly substantial (0.68) because excluded claimants and cognatic territories contest the doctrine's validity at every succession crisis, frequently through war.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic male claimants and the noble houses tied to their fortunes sit near the full-beneficiary end: the rule manufactures their claims out of birth order alone and forecloses rival claims before they can be weighed. Agnatic-line nobility and royal jurists/clergy occupy the agenda-setter seat — they administer and elaborate the doctrine and derive institutional standing from its perpetuation, which is a distinct form of benefit from direct inheritance. Female heirs and their issue sit at the full-target end: trapped exit, no internal appeal, categorical rather than partial exclusion. Cognatic claimant territories sit closer to target than beneficiary but retain some exit via external war or negotiated settlement, hence 'constrained' rather than 'trapped.' Subjects of contested successions are diffuse payers who bear the downstream cost (war, instability) without holding a claim themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — foreclosing succession litigation to prevent civil war — was real in the 14th-century Frankish successor context but the immutable-mandate reading launders that transitional, contingent settlement into a permanent, unamendable feature of natural/divine order. Classifying this reading as tangled_rope rather than mountain or pure snare preserves both halves honestly: there IS a genuine coordination problem (reducing succession litigation) that a bright-line rule solves more cheaply than case-by-case adjudication, AND the specific agnatic-exclusive form of that rule asymmetrically transfers value to one class at categorical cost to another, requiring active suppression (denial of appeal, war) to hold. Calling it a mountain would launder the extraction as natural law; calling it a pure snare would erase the genuine reduction in succession-dispute frequency the rule does deliver for the claimants it does recognize.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_manufactured_doctrine,
    'Is agnatic-exclusive succession a genuine feature of a pre-existing ''fundamental law of the realm,'' or was the ''immutable and antecedent'' framing itself constructed after the fact — chiefly during the 1316-1328 French succession disputes and later elaborated by 15th-16th century jurists — to settle a specific contested inheritance and then retrofitted with ancient and natural-law pedigree?',
    'Comparative textual and legal-historical analysis of the earliest invocations of the exclusion versus later ''fundamental law'' treatises; dating when ''natural/divine law'' language first attaches to the succession rule relative to when concrete succession disputes required it.',
    'If manufactured, this reading is a false summit: an extraction-serving doctrine dressed as discovered natural law, strengthening the case for classifying it as tangled_rope (or even snare) rather than any variant approaching mountain. If genuinely ancient and consistently applied prior to any contested case, the natural-law claim gains more credibility, though the categorical-exclusion extraction would remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_manufactured_doctrine, empirical, 'Whether the ''immutable natural law'' framing is genealogically prior to or manufactured for a specific succession dispute.').

omega_variable(
    kernel_reading_selection_pressure,
    'Among the three readings of the Salic prohibition kernel (immutable_mandate, sovereign_override, cognatic_reversion), which reading a given dynastic house or claimant invokes appears to track which reading currently favors their claim rather than any independent jurisprudential commitment — is reading-selection itself strategic?',
    'Track whether individual houses or jurists switch which reading they endorse when their own genealogical position shifts (e.g., a house that invoked cognatic_reversion to press a female-line claim later invoking immutable_mandate to block a rival''s female-line claim).',
    'If reading-selection is strategic and inconsistent per-actor, it strongly supports treating all three readings as instrumentally deployed cover stories for extraction rather than genuine competing jurisprudential traditions — though the constraint stories themselves remain ε-invariant per reading regardless of how instrumentally any given actor deploys them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether historical actors'' choice among sibling readings tracks genealogical self-interest rather than consistent doctrine.').

omega_variable(
    preventive_war_legitimacy_threshold,
    'Under the immutable-mandate reading, at what point does ''enforcing agnatic priority'' shade from legitimate defense of a settled constitutional order into naked conquest using the doctrine as pretext?',
    'Case-by-case examination of specific succession wars (e.g., Hundred Years'' War, War of the Spanish Succession) for proportionality between the strength of the excluded claim and the scale of military response.',
    'If preventive wars under this reading are consistently disproportionate to the strength of the excluded claim, it supports a higher suppression/extraction reading of the doctrine''s actual function versus its stated coordination purpose.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preventive_war_legitimacy_threshold, preference, 'Where the line falls between legitimate constitutional defense and extraction-driven war under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(sali_tr_t0, observed).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__immutable_mandate_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(sali_tr_t20, observed).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__immutable_mandate_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(sali_tr_t40, observed).
narrative_ontology:measurement(sali_tr_t60, salic_prohibition__immutable_mandate_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement_basis(sali_tr_t60, observed).
narrative_ontology:measurement(sali_tr_t80, salic_prohibition__immutable_mandate_reading, theater_ratio, 80, 0.39).
narrative_ontology:measurement_basis(sali_tr_t80, observed).
narrative_ontology:measurement(sali_tr_t100, salic_prohibition__immutable_mandate_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(sali_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(sali_be_t0, observed).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__immutable_mandate_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(sali_be_t20, observed).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__immutable_mandate_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(sali_be_t40, observed).
narrative_ontology:measurement(sali_be_t60, salic_prohibition__immutable_mandate_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(sali_be_t60, observed).
narrative_ontology:measurement(sali_be_t80, salic_prohibition__immutable_mandate_reading, base_extractiveness, 80, 0.69).
narrative_ontology:measurement_basis(sali_be_t80, observed).
narrative_ontology:measurement(sali_be_t100, salic_prohibition__immutable_mandate_reading, base_extractiveness, 100, 0.71).
narrative_ontology:measurement_basis(sali_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(sali_su_t0, observed).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__immutable_mandate_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(sali_su_t20, observed).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__immutable_mandate_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(sali_su_t40, observed).
narrative_ontology:measurement(sali_su_t60, salic_prohibition__immutable_mandate_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement_basis(sali_su_t60, observed).
narrative_ontology:measurement(sali_su_t80, salic_prohibition__immutable_mandate_reading, suppression_requirement, 80, 0.78).
narrative_ontology:measurement_basis(sali_su_t80, observed).
narrative_ontology:measurement(sali_su_t100, salic_prohibition__immutable_mandate_reading, suppression_requirement, 100, 0.8).
narrative_ontology:measurement_basis(sali_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language label 'Salic Law' per the eps-invariance principle. Each reading of the salic_prohibition kernel is authored as a separate constraint with its own eps, beneficiary/victim structure, and classification: immutable_mandate_reading (this story, tangled_rope, eps=0.71) treats the rule as irrevocable fundamental law; sovereign_override_reading treats it as ordinary revisable statute; cognatic_reversion_reading denies the rule ever validly bound non-Frankish territories. The three share the same underlying historical kernel text and dispute but produce structurally distinct claims about revisability and territorial scope, hence distinct eps values and distinct victim sets — they are linked via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
