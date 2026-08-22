% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Salic Prohibition as Immutable Dynastic Law
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Salic Law, a legal rule originating in Frankish succession practice,
 *   excludes female heirs from inheritance to the throne. Under the
 *   immutable-mandate reading instantiated in this constraint story, the
 *   Salic prohibition is presented as irrevocable natural law or divine
 *   commandment embedded in the realm's dynastic constitution — not as a
 *   policy choice the sovereign could alter. Female heirs are categorically
 *   barred; challengers to female succession (including preventive military
 *   action) are justified by appeal to natural/divine order. This reading
 *   vests the prohibition in transcendent authority (nature, divinity,
 *   constitutional immutability) rather than in current sovereign will,
 *   making it appear unchangeable. The immutable-mandate reading is ONE of
 *   three contested framings of the same historical Salic rule (kernel:
 *   salic_prohibition); sibling readings present it as revocable positive law
 *   (sovereign_override_reading) or as anachronistic Frankish custom
 *   inapplicable outside Frankland (cognatic_reversion_reading). This
 *   constraint story generates the immutable-mandate reading only; the
 *   siblings are separate constraints linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - agnatic_male_heirs: Primary beneficiary; male-line succession is vested in them as natural right
 *   - female_heirs: Primary target; categorically excluded from succession regardless of proximity or claim
 *   - realm_administrators: Agenda-setter; enforce the prohibition against female claimants and suppress challenges
 *   - patrilineal_legitimacy_doctrine: Vindicated proposition (non-agent); the Salic reading vindicates the doctrine that political legitimacy derives from patrilineal descent
 *   - female_claimants_and_supporters: Excluded voices; would challenge the rule's naturalness but are framed as rebels against natural order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.81).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.88).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, mountain).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Prohibition as Immutable Dynastic Law").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional/political").

domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'b033c352-09f1-4e85-9d1d-59796c3f5628').
narrative_ontology:cs_kernel_codification('b033c352-09f1-4e85-9d1d-59796c3f5628', fixed_text).
narrative_ontology:cs_authority_grounding('b033c352-09f1-4e85-9d1d-59796c3f5628', lineage).
narrative_ontology:cs_interpretation_layer_present('b033c352-09f1-4e85-9d1d-59796c3f5628').
narrative_ontology:cs_reading_relation('b033c352-09f1-4e85-9d1d-59796c3f5628', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('b033c352-09f1-4e85-9d1d-59796c3f5628', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('b033c352-09f1-4e85-9d1d-59796c3f5628', foundational, agnatic_descent_natural_law).
narrative_ontology:cs_axiom_status(agnatic_descent_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('b033c352-09f1-4e85-9d1d-59796c3f5628', agnatic_descent_natural_law, deontological).
narrative_ontology:cs_axiom('b033c352-09f1-4e85-9d1d-59796c3f5628', foundational, female_succession_violates_natural_order).
narrative_ontology:cs_axiom_status(female_succession_violates_natural_order, holdable).
narrative_ontology:cs_axiom_grounding('b033c352-09f1-4e85-9d1d-59796c3f5628', female_succession_violates_natural_order, theological).
narrative_ontology:cs_reference_frame('b033c352-09f1-4e85-9d1d-59796c3f5628', agnatic_primogeniture_natural_order).
narrative_ontology:cs_drift_state('b033c352-09f1-4e85-9d1d-59796c3f5628', post_enlightenment_sovereign_authority_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b033c352-09f1-4e85-9d1d-59796c3f5628', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, patrilineal_legitimacy_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Male-line succession guarantees inheritance within their patrilineal cohort. The Salic reading vests their claim in natural law rather than sovereign will, making their right appear inalienable and beyond legislative revision. They benefit from the categorical exclusion of women because it compresses the eligible pool and reinforces the claim that male succession is ordained, not chosen.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs, beneficiary,
    institutional, generational, arbitrage, continental).

% Categorically excluded from succession regardless of proximity to the throne, direct claim, or absence of male alternatives. Under the immutable-mandate reading, their exclusion is presented as natural law rather than as a policy choice the sovereign could reverse. Their identity as heirs is locked to a bloodline that the rule deems ineligible — they cannot escape the exclusion by redefining themselves.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs, payer,
    powerless, generational, identity_locked, continental).

% Enforce the Salic exclusion against female claimants and against vassals or neighboring powers that would support female succession. Their enforcement burden increases when a succession crisis involves a strong female claimant; they must actively prevent her accession and suppress rebellion from those who view her claim as legitimate. The immutable-mandate framing shifts the burden from 'we choose male succession' to 'we enforce natural law,' which simplifies enforcement but requires that challenges be framed as defiance of nature rather than disagreement with policy.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, realm_administrators, agenda_setter,
    institutional, generational, constrained, continental).

% A normative claim (not an actor) that political legitimacy derives from unbroken patrilineal descent and that such descent is a natural or divine fact, not a human convention. The Salic reading vindicates this doctrine by treating female exclusion as a consequence of nature, not as a sovereign policy choice. The doctrine collects no rents but provides the epistemic frame that makes the extraction appear natural rather than coercive.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, patrilineal_legitimacy_doctrine, beneficiary,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(salic_prohibition__immutable_mandate_reading, patrilineal_legitimacy_doctrine).

% Would argue that female proximity to the throne, absence of viable male heirs, or changed circumstances should override the Salic rule, or that the rule is itself a Frankish anachronism not binding on the realm. They are excluded from the conversation that defines legitimacy; their objection is treated as a challenge to natural order rather than as a policy proposal. Any female claimant must either accept the naturalness of her own exclusion or risk being branded a rebel against divine law.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_claimants_and_supporters, excluded,
    powerless, biographical, trapped, continental).

% May exploit succession disputes (especially when a strong female claimant is barred) by supporting her claim to destabilize the realm or advance their own dynasty's position. From their external vantage, the Salic rule is a constraint they can leverage rather than obey; they observe how strictly it is enforced and whether domestic actors accept it as natural law or merely as current policy.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, rival_dynastic_powers, observer,
    institutional, generational, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, transparent rule for succession that eliminates ambiguity about who inherits: by restricting to male-line descent, the rule creates certainty in a system where disputed succession is the primary cause of civil war and invasion. A clear male-succession rule is easier to defend against foreign claimants and avoids the factional fragmentation that would emerge if multiple female claimants had competing claims.
% TRANSFER_FUNCTION: Transfers the right to rule from the set of all heirs (which would include both male and female children and their descendants) to the set of male-line descendants only. This move benefits agnatic heirs and the patrilineal doctrine at the cost of female heirs, whose inheritance rights are permanently erased.
% ABSENT_VOICES: Female claimants and their supporters are structurally excluded from the conversation; they are the voices that would object to the rule's naturalness. Women of the realm who might inherit if the rule were reversed, powerful women who might support female succession, and cognatic-tradition advocates from non-Frankish territories are kept outside the frame.
% DISAPPEARANCE_RATIONALE: If the Salic prohibition disappeared, succession would immediately be contested along cognatic lines (including female heirs), realm boundaries would become unstable as rival powers supported multiple claimants, and the administrative focus would shift from enforcing male-only succession to negotiating among multiple legitimate contenders. The constraint's removal would reorder the succession system entirely.
% FOUNDING_PROBLEM: Early Frankish succession practices were pluralistic and unstable; multiple male sons inherited divided territory, and female inheritance was locally variable, leading to repeated civil wars. The Salic reading claims to solve this by establishing an irrevocable, divinely sanctioned rule: only males inherit, descent is unambiguous, and the rule is beyond sovereign revision.
% FOUNDING_PROBLEM_CORROBORATION: Modern legal historians document that Frankish succession was indeed contested and unstable in the early medieval period; the Salic rule did reduce some ambiguity around male succession. However, contemporary sources and later historical analysis (from outside the patrilineal-legitimacy constituency) show that female succession continued in practice in many realms, that the 'immutable' claim was invoked inconsistently to prevent female inheritance specifically when it threatened the reigning dynasty, and that the rule persisted long after the founding destabilization problem had been solved by other institutional developments (primogeniture, centralized monarchy). The immutability claim is attested only by benefiting parties and is contradicted by the rule's selective enforcement and documented revision in practice.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because the rule transfers inheritance rights from a larger set (all heirs by various rules) to a smaller set (male-line only), and the transfer is permanent and non-negotiable under the immutable reading. Suppression is very high (0.88) because the constraint's persistence depends on active enforcement — preventing female claimants from inheriting, suppressing their supporters, and defeating challenges to the rule. Theater ratio is moderate-high (0.62 at interval end) and rising over the interval, indicating that the proportion of enforcement activity devoted to defending the rule's immutability (rather than administering succession itself) increases as the rule is challenged. Accessibility collapse is very high (0.92) because once the prohibition is understood as natural or divine law, female claimants have no available alternative path to succession within the same frame; they must either accept their exclusion as natural or reject the entire legitimacy framework. Resistance is moderate (0.44) because female claimants and their supporters do resist the rule, but their resistance is reframed by beneficiaries as rebellion against divine order rather than as legitimate policy disagreement, which dampens the political viability of the resistance. The measurement series tracks rising extractiveness and theater ratio over time, indicating that as succession crises arise and female claimants mount more serious challenges, the claim to immutability becomes more central to the rule's enforcement — the theater component (naturalness justification) grows as structural suppression alone becomes insufficient. The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter (realm administrators) and beneficiary (agnatic heirs) seats, the Salic prohibition appears as a natural law they inherit and defend — their role is to enforce an order that precedes them. From the female-heir seat, the same rule appears as active, chosen suppression justified post-hoc by an appeal to naturalness. The engine computes this divergence from structural data: agnatic heirs have high power and unquestioned access to succession (low directionality toward the constraint's suppressive force), while female heirs have trapped exit and bear the full weight of the prohibition (high directionality toward extraction). This structural asymmetry should produce different per-seat classifications: for agnatic heirs, the constraint might compute as coordination (they have no real alternative and benefit from predictable succession); for female heirs, it computes as snare (they are permanently excluded with no exit). The immutable-mandate reading presents the extraction as natural/inevitable to obscure this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic male heirs benefit directly from the prohibition (they inherit when they otherwise would compete with female claimants); their directionality is low (d near 0.0 — beneficiary end). Female heirs bear the full cost of the prohibition (they are excluded from succession entirely); their directionality is high (d near 1.0 — target end). Realm administrators are positioned as enforcers of a natural law, which places them at the agenda-setter role but gives them moderate directionality: they enforce the rule, but they also benefit from the clarity and certainty it provides (they face less factional pressure if succession is unambiguous). The patrilineal-legitimacy doctrine is not an actor (agent: false) and does not have directionality; it is a vindicated proposition that provides the epistemic frame making the extraction appear natural. Female claimants and their supporters are excluded, not coordinated — they would benefit from the rule's reversal, but they are not in the conversation about what the rule means or whether it is natural.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early Frankish succession instability) is declared dead — other institutional developments (primogeniture, centralized monarchy, bureaucratic administration) solved the succession-clarity problem that the Salic rule was invoked to address. The constraint persists after its founding problem has been solved, but it persists because it benefits agnatic heirs (extraction layer) and because the immutable-mandate framing makes revision unthinkable. The theater ratio rising over time (from 0.45 to 0.62) indicates that as the actual succession-function problem became less pressing, the proportion of enforcement activity devoted to the rule's *naturalness* (theatrical maintenance of the immutability claim) increased relative to the actual coordination work. A mandatrophy flag should fire on the (founding_problem_status=dead, disappearance_verdict=world_rearranges) mismatch: the world would rearrange if the rule vanished, but the founding problem that justified it is gone. The constraint persists as pure extraction riding on a defunct coordination rationale — that is the mandatrophy case. The immutable-mandate reading is especially vulnerable to mandatrophy diagnosis because it vests the rule in transcendent authority, making it harder for beneficiaries to admit that the founding coordination problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_policy,
    'Is the Salic prohibition a genuine natural or divine law (mountain), or a constructed dynastic policy that benefits agnatic heirs and instrumentally invokes naturalness to resist revision?',
    'Historical-comparative analysis: if the rule appears in diverse, independent legal traditions without transmission, it has weight as natural law; if it is unique to patrilineal dynasties and is selectively enforced to block female succession specifically when it threatens reigning male heirs, it is constructed policy masquerading as natural law. Post-succession-crisis observations: if female claimants are barred and their supporters reframe the rule as revocable policy, and if male beneficiaries then reassert its immutability, the beneficiary-defense pattern indicates construction rather than naturalness.',
    'If the prohibition is natural law, female exclusion is inevitable and unjust resistance is futile. If it is constructed policy, its characterization as immutable is false, and sovereign authority could revoke it — the classification shifts from mountain to tangled_rope or snare (the extraction layer becomes visible). The immutable-mandate reading depends on naturalness; if that fails, the reading itself is invalidated and cognatic or sovereign-override readings become viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_policy, conceptual, 'Whether the Salic prohibition is a feature of natural/divine order or a human policy choice strategically labeled immutable.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Does female acceptance of the Salic prohibition rest on belief in its naturalness (internalized suppression), or on structural barriers (law, enforcement, social exclusion from inheritance) that would persist if the naturalness claim were abandoned?',
    'If a sovereign publicly rejected the immutability claim and offered female succession, would female claimants and their supporters immediately mobilize to contest inheritance? If yes, suppression is partly structural and would persist; if suppression dissolves once the naturalness claim is abandoned, it was primarily internalized. Counter-test: If female claimants mount challenges on the basis that the rule is not natural/divine but constructable, and if the suppression infrastructure (enforcement, legal bars, social ostracism) activates against them without requiring the naturalness justification, suppression is structural.',
    'If suppression is primarily internalized, the effective suppression (χ) can be reduced by epistemic intervention (challenging the naturalness claim); if structural, the constraint is harder to destabilize. High structural suppression would support reclassification toward snare; internalized suppression alone might leave it as tangled_rope. The theater ratio rises when naturalness claims carry more of the suppressive load than enforcement does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of female succession is cognitive/internalized or backed by enforcement infrastructure.').

omega_variable(
    immutability_invoked_selectively,
    'Is the immutability claim invoked consistently whenever Salic succession is discussed, or selectively invoked when female claimants threaten inheritance and downplayed when convenient for male heirs?',
    'Content analysis of appeals to the Salic rule: count invocations of ''immutable/natural/divine'' language when blocking female succession vs. when male beneficiaries negotiate exceptions for their own line. If immutability language is dominant in female-succession contexts and marginal in male-line disputes, the rule is instrumentally deployed rather than genuinely held as immutable.',
    'Selective invocation of immutability to block female succession while treating male succession as negotiable undermines the naturalness claim and suggests the rule is a policy instrument for protecting agnatic interests. This would support reclassification to false-summit mountain (constructed rule masquerading as natural) and downstream reclassification to snare or tangled_rope once the false summit is detected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immutability_invoked_selectively, empirical, 'Whether immutability is asserted consistently or strategically deployed against female succession.').

omega_variable(
    kernel_reading_contest,
    'The salic_prohibition kernel admits three structurally distinct readings. Is immutable-mandate the enduring reading across jurisdictions, or do cognatic-reversion and sovereign-override readings remain live alternatives in contestation?',
    'Historical record of succession disputes: when female claimants arose, did benefiting parties consistently invoke immutable-mandate framing, or did different constituencies deploy different readings? Did some realms accept cognatic succession while invoking immutability elsewhere? Did some sovereigns exercise override authority while others deferred to immutability? The distribution of reading invocations across time, jurisdiction, and factional position indicates which readings are genuinely live vs. which are rhetorical moves.',
    'If immutable-mandate is the sole reading invoked and defended, the immutable-mandate constraint story is the primary analysis. If all three readings remain live (different parties deploy different frames for the same Salic rule), the kernel contest is unresolved and all three constraint stories are operative — the three readings coexist as competing frames rather than one being the natural reading. This affects the status of the immutable-mandate reading: if it is one frame among live alternatives, it is not THE frame of the Salic prohibition; if it is the enduring reading, other readings are overcome or suppressed frames.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which reading(s) of the salic_prohibition kernel remain live in historical contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(sali_tr_t0, observed).
narrative_ontology:measurement(sali_tr_t5, salic_prohibition__immutable_mandate_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement_basis(sali_tr_t5, observed).
narrative_ontology:measurement(sali_tr_t10, salic_prohibition__immutable_mandate_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement_basis(sali_tr_t10, observed).
narrative_ontology:measurement(sali_tr_t15, salic_prohibition__immutable_mandate_reading, theater_ratio, 15, 0.56).
narrative_ontology:measurement_basis(sali_tr_t15, observed).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__immutable_mandate_reading, theater_ratio, 20, 0.59).
narrative_ontology:measurement_basis(sali_tr_t20, observed).
narrative_ontology:measurement(sali_tr_t25, salic_prohibition__immutable_mandate_reading, theater_ratio, 25, 0.61).
narrative_ontology:measurement_basis(sali_tr_t25, observed).
narrative_ontology:measurement(sali_tr_t30, salic_prohibition__immutable_mandate_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement_basis(sali_tr_t30, observed).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__immutable_mandate_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(sali_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(sali_be_t0, observed).
narrative_ontology:measurement(sali_be_t5, salic_prohibition__immutable_mandate_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(sali_be_t5, observed).
narrative_ontology:measurement(sali_be_t10, salic_prohibition__immutable_mandate_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement_basis(sali_be_t10, observed).
narrative_ontology:measurement(sali_be_t15, salic_prohibition__immutable_mandate_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement_basis(sali_be_t15, observed).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__immutable_mandate_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(sali_be_t20, observed).
narrative_ontology:measurement(sali_be_t25, salic_prohibition__immutable_mandate_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement_basis(sali_be_t25, observed).
narrative_ontology:measurement(sali_be_t30, salic_prohibition__immutable_mandate_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(sali_be_t30, observed).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__immutable_mandate_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(sali_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement_basis(sali_su_t0, observed).
narrative_ontology:measurement(sali_su_t5, salic_prohibition__immutable_mandate_reading, suppression_requirement, 5, 0.84).
narrative_ontology:measurement_basis(sali_su_t5, observed).
narrative_ontology:measurement(sali_su_t10, salic_prohibition__immutable_mandate_reading, suppression_requirement, 10, 0.86).
narrative_ontology:measurement_basis(sali_su_t10, observed).
narrative_ontology:measurement(sali_su_t15, salic_prohibition__immutable_mandate_reading, suppression_requirement, 15, 0.87).
narrative_ontology:measurement_basis(sali_su_t15, observed).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__immutable_mandate_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement_basis(sali_su_t20, observed).
narrative_ontology:measurement(sali_su_t25, salic_prohibition__immutable_mandate_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement_basis(sali_su_t25, observed).
narrative_ontology:measurement(sali_su_t30, salic_prohibition__immutable_mandate_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement_basis(sali_su_t30, observed).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__immutable_mandate_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement_basis(sali_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(salic_prohibition__immutable_mandate_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% The salic_prohibition kernel contains three structurally distinct readings. This constraint instantiates the immutable-mandate reading: Salic Law as irrevocable natural/divine law. The sovereign_override_reading frames it as revocable positive law. The cognatic_reversion_reading frames it as anachronistic Frankish custom. Each reading has different ε, different beneficiary/victim structure, and different classification. The immutable reading should compute as mountain or false-summit mountain (if beneficiary presence triggers FSM); the sovereign reading should compute as tangled_rope or snare (the law is a policy tool, not a natural fact). The cognatic reading reframes the kernel itself (what counts as the Salic prohibition in non-Frankish territories?) and may not apply in some jurisdictions. Each story is independent; the network links show how the kernel contest structures downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
