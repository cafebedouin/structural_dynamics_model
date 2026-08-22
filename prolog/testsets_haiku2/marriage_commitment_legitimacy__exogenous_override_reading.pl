% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: Federal Override of Polygamous Marriage Doctrine (Exogenous Coercion Reading)
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto, issued by LDS Church President Wilford Woodruff,
 *   announced the cessation of plural marriage in response to federal
 *   pressure: territorial statehood was contingent on abandoning the
 *   practice, Church properties were under federal seizure threat, and
 *   practitioners faced criminal prosecution. On this reading (exogenous
 *   override), the Manifesto represents institutional capitulation under
 *   coercion, not theological development. The theological doctrine of plural
 *   marriage (understood by the Church as eternally mandated for salvation)
 *   was not abandoned—only practice was suspended under duress. This creates
 *   a structural legitimacy crisis: members must navigate the gap between
 *   claimed doctrine (prophetic revelation) and empirical reality (federal
 *   force as the mechanism of change). The constraint extracts institutional
 *   compliance from the Church and doctrinal abandonment from members, while
 *   the federal government collects the benefit of territorial consolidation
 *   and normalized institutional compliance.
 *
 * KEY AGENTS:
 *   - Federal government: institutional power seat, agenda-setter, benefits from extracting doctrinal compliance and territorial control
 *   - LDS Church leadership: institutional power seat, agenda-setter under duress, benefits from organizational survival but pays cost of legitimacy crisis
 *   - LDS membership (general): organized power seat, victims of doctrinal contradiction and identity alienation, identity-locked exit
 *   - Polygamist practitioners: powerless seat, trapped, bear the cost of family dissolution and spiritual abandonment
 *   - Federal courts: institutional enforcer of the constraint, adjudicate property and criminal matters
 *   - Mainstream Protestant denominations: institutional beneficiaries of LDS normalization into American Protestant landscape
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.82).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.76).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Federal Override of Polygamous Marriage Doctrine (Exogenous Coercion Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, 'ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51').
narrative_ontology:cs_kernel_codification('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', formalized).
narrative_ontology:cs_authority_grounding('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', extraction).
narrative_ontology:cs_interpretation_layer_present('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51').
narrative_ontology:cs_reading_relation('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', foundational, federal_coercion_primary_driver).
narrative_ontology:cs_axiom_status(federal_coercion_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', federal_coercion_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', foundational, doctrine_unchanged_practice_suspended).
narrative_ontology:cs_axiom_status(doctrine_unchanged_practice_suspended, holdable).
narrative_ontology:cs_axiom_grounding('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', doctrine_unchanged_practice_suspended, deontological).
narrative_ontology:cs_reference_frame('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', eternal_plural_marriage_doctrine).
narrative_ontology:cs_drift_state('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', post_manifesto_enforcement_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ea42676a-ceb3-4bc1-8e4b-70eb7f41cc51', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, polygamist_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, non_polygamist_church_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, mainstream_protestant_denominations).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, reform_faction_within_lds).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, non_lds_american_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses statehood conditionality, property seizure, and criminal prosecution to compel doctrinal compliance. Benefits from territorial consolidation and institutional normalization. Sets the terms unilaterally; the Church cannot refuse without territorial consequences.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Faces legitimacy crisis between declared doctrine (plural marriage as salvific) and enforced practice (plural marriage prohibited). Bears the cost of the theological contradiction and the institutional abandonment of the original doctrine. Identity-lock means exit requires severing family, community, and self-concept (abandoning religious identity entirely).
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership, payer,
    organized, generational, identity_locked, regional).

% Practitioners who entered plural marriage under doctrine now face criminal prosecution, property loss, and institutional abandonment. Trapped: continuing the practice incurs legal consequences; ending it incurs spiritual consequences. Escape requires leaving the religious community entirely (identity loss) or accepting that their marriages are spiritually invalid.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, polygamist_practitioners, payer,
    powerless, biographical, trapped, regional).

% Enforce legal interdicts against plural marriage and adjudicate Church property claims. Provide the enforcement machinery that gives federal coercion credibility. Frame the doctrinal dispute as a law-and-order matter, translating religious practice into legal violation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, federal_courts, observer).

% Announce and administer the doctrine reversal. Benefit from institutional preservation and federal recognition. Constrained by the requirement to present the reversal as divinely revealed while managing member alienation and the obvious contradiction between the claimed revelation and the manifest federal pressure.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, non_polygamist_church_leadership, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, non_polygamist_church_leadership, agenda_setter).

% Benefit from LDS normalization into American Protestantism. The doctrinal reversal removes the primary theological distinction. No enforcement role but social pressure and federal alignment reinforce the constraint.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, mainstream_protestant_denominations, beneficiary,
    institutional, generational, mobile, national).

% Faction uncomfortable with plural marriage on ethical grounds. The Manifesto provides doctrinal relief without challenging Church authority. Benefits from alignment with mainstream values while remaining institutionally positioned. Must navigate the tension that relief comes from federal coercion, not theological rethinking.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, reform_faction_within_lds, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, reform_faction_within_lds, observer).

% General public opposes plural marriage on moral and social grounds. Benefits from doctrinal abandonment and alignment of law with public values. Benefit is diffuse (cultural alignment rather than material gain).
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, non_lds_american_public, beneficiary,
    organized, biographical, mobile, national).

% The institutional claim that the Church President receives continuing divine revelation. This doctrine is both affirmed (Manifesto presented as prophetic) and undermined (widely read as coerced). Excluded voices would argue genuine prophetic authority cannot be overridden by temporal government; allowing this voice would force acknowledging the contradiction.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, prophetic_authority_doctrine, excluded,
    powerful, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__exogenous_override_reading, prophetic_authority_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The LDS Church's doctrinal authority originally coordinated religious identity with family practice; plural marriage doctrine provided theological justification for an alternative family structure. The Manifesto purports to coordinate the same identity function through reversed practice, but on this reading the coordination is breached because the reversal is externally imposed rather than theologically continuous. The actual coordination function that persists is institutional survival under federal constraint—the Manifesto solves the federal problem, not a genuinely internal coordination problem.
% TRANSFER_FUNCTION: Moves institutional legitimacy FROM the LDS Church TO the federal government. The Church sacrifices its distinctive theological doctrine in exchange for territorial admission and property preservation. Moves prophetic authority claims into a vehicle for federal objectives—the Manifesto becomes an instrument of federal power rather than an independent theological authority. Transfers the legitimacy cost TO LDS members, who must reconcile theological understanding of doctrine (unchanging, divinely mandated) with empirical reality of capitulation under federal threat.
% ABSENT_VOICES: Polygamist practitioners and their families are excluded from decision-making. Prophetic authority claims (voices arguing genuine revelation cannot be federally coerced) are silenced by the institutional framing that presents the Manifesto as prophetic. Members who understand plural marriage as core to spiritual identity have no institutional voice—objections can be made only through leaving the Church. Reform factions breaking away to continue plural marriage practice are institutionally expelled, removing their voices from the Church's ongoing negotiation.
% DISAPPEARANCE_RATIONALE: If the federal constraint disappeared, the Church would face immediate choice between reasserting plural marriage doctrine or maintaining the reversal. On this reading the constraint sustains the reversal; without it, the theological doctrine would likely reassert because it was never truly abandoned—only practice was suspended under duress. The institutional outcome (prohibition of plural marriage) is structural to the constraint; its removal would reopen the theological question and likely trigger realignment.
% FOUNDING_PROBLEM: Federal government sought to consolidate territorial control by eliminating practices classified as socially deviant. Plural marriage was the primary institutional marker distinguishing LDS communities from mainstream American society. Federal authorities perceived plural marriage under religious sanction as a governance problem resistant to federal legal authority.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislators and courts attested a governance problem requiring institutional compliance. The LDS Church leadership attests the problem was theological (prophetic redirection). On this reading neither is trustworthy: federal government had structural interest in consolidation, Church had interest in survival. Independent historians outside both institutions (Jan Shipps, Sarah Barringer Gordon, Kathryn Daynes) document the constraint as explicitly coercive: federal pressure was the primary driver of institutional reversal, not theological rethinking. The founding problem was federal determination to eliminate the practice, not a preexisting crisis.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the federal government collects institutional compliance (the primary gain) while the Church's core theological doctrine is overridden. The Manifesto is presented as prophetic, but the timing (announced under federal pressure), the content (reversing a doctrine declared eternal just decades prior), and the mechanism (institutional survival contingent on reversal) all mark this as exogenously imposed extraction. Suppression is high (0.76) because the constraint's persistence depends on continuous enforcement: ongoing property seizure threat, criminal prosecution of practitioners, and institutional pressure on the Church to publicly affirm the reversal. Theater ratio is moderately elevated (0.58) because a growing share of the Church's enforcement effort goes into managing the legitimacy contradiction rather than the original coordination function. The constraint presents itself as religious (prophetic revelation) but operates as political (federal coercion); this gap between the theatrical claim and the mechanism is what theater_ratio captures. Accessibility_collapse is moderate (0.64) because members cannot exit the Church and rejoin without identity loss, but alternatives do exist (leaving the Church entirely, or joining reform sects that broke off in protest). Resistance is high (0.71) because significant constituencies within the Church resisted the Manifesto—practitioners continued plural marriage for decades after, and members disputed whether the reversal was truly prophetic. The measurement series tracks the constraint hardening over 50 time units: extractiveness and suppression_requirement increase as the federal threat is sustained and the Church is forced to defend the reversal institutionally; theater_ratio rises as the gap between the claimed prophetic status and the empirical coercion becomes undeniable to more members, forcing more elaborate theological performances to justify it.
 *
 * PERSPECTIVAL GAP:
 *   The federal government sits at a nearly pure beneficiary position (d ≈ 0.05): they extract compliance without bearing the legitimacy costs. The Church leadership sits near the midpoint (d ≈ 0.45): they benefit from institutional survival but pay in legitimacy erosion and member alienation. The polygamist practitioners sit at nearly pure target position (d ≈ 0.95): they bear the full cost of family dissolution, legal prosecution, and spiritual abandonment. This steep gradient drives different type perceptions: from the federal seat, the arrangement is a successful institutional normalization (rope-flavored coordination); from the Church leadership seat, it is a forced adaptation that maintains continuity (some coordination function remains, but under duress); from the victim seat, it is pure extraction (theological abandonment, legal persecution, family loss). The engine computes these differently per seat because the structural relationships are asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Federal government is the identified beneficiary (extractiveness target); they have institutional power, arbitrage options (multiple territories, multiple institutional partners), and zero identity-lock. They derive d ≈ 0.05 (full beneficiary, minimal cost). LDS membership is identified as victim (declared in base_properties.victims[]); they are organized but not institutional, have generational time horizon, and are identity_locked (leaving the Church means severing family, community, and self-concept). Identity-lock elevates their d substantially upward from what mobility alone would imply—they cannot exit without existential cost. This produces d ≈ 0.85–0.90 (near-target). Polygamist practitioners are declared as victims; they are powerless, trapped (no legal way to continue the practice, no way to exit the community without abandoning family), and identity-locked. Their d ≈ 0.95 (full target). Church leadership is neither beneficiary nor victim in the base_properties declaration—they are the agent_setter who cooperates with federal coercion. Their d is derived from their dual role: they benefit from institutional survival (d-lowering) but are constrained by federal threat (d-raising). Default derivation would place them around 0.45–0.55 (mixed). No override is needed; the structural data produce the correct asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The Manifesto represents a live case of mandate obsolescence: the original mandate was to coordinate religious identity with family practice (plural marriage as salvific pathway). The founding problem it solved was providing theological legitimacy for an alternative family structure within an American context where that structure was culturally and legally aberrant. The federal constraint overrides this mandate entirely—not by persuading the Church to reinterpret the doctrine theologically, but by making it materially impossible to practice. The Manifesto's status as 'solution' is therefore inverted: it claims to solve a new problem (federal demand for compliance) by abandoning the solution to the old problem (theological legitimacy). This is classic mandatrophy: the arrangement persists (plural marriage prohibition is now Church law) but the justification has shifted from theological (divine mandate) to coerced (federal threat). The theater_ratio's rise from 0.35 to 0.58 tracks this: over time, the Church must engage in more elaborate performance to justify why the 'eternal doctrine' was actually suspended—claims about 'lower laws' vs. 'higher laws,' assertions that the spiritual principle remains even if practice is suspended, retrospective reinterpretations of foundational scriptures. This performative escalation is the signature of mandate obsolescence: the original coordination function (making plural marriage theologically mandatory for salvation) is dead, but the institutional arrangement persists through theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_coercive_causation,
    'Is the Manifesto primarily a theological development (revelation) or primarily a coerced institutional response? Can it be both simultaneously without one being the truthful account and the other a cover story?',
    'Comparison of counterfactual: absent federal pressure and statehood conditionality, would the Church have issued a similar doctrinal reversal on its own theological trajectory? Archival evidence of internal Church debate before and after federal pressure can distinguish between theological rethinking and strategic compliance.',
    'If primarily coercive: the constraint is a snare with high extractiveness; theological legitimacy is instrumentalized. If primarily theological: the constraint is a rope or scaffold with genuine coordination function; the federal context is enabling rather than forcing. The reading depends on causation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_coercive_causation, empirical, 'Whether theological development or political coercion is the primary driver of the Manifesto.').

omega_variable(
    doctrine_vs_practice_distinction_coherence,
    'Can the distinction between ''doctrine unchanged, practice suspended'' be maintained coherently? Does suspending practice indefinitely in response to external threat constitute a real change in doctrine?',
    'Examine subsequent Church teachings: if the doctrine is truly unchanged, doctrine should be reasserted when pressure is removed or waived. If the doctrine remains suspended despite lifting of federal pressure (as in fact occurred), the distinction collapses and the suspension becomes the new doctrine.',
    'If the distinction is coherent: members can maintain that their theological commitments are intact even if practice is impossible. If the distinction collapses: members must acknowledge doctrinal abandonment, not mere practice suspension. This determines whether the victim set experiences identity loss or merely temporary coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_vs_practice_distinction_coherence, conceptual, 'Whether the doctrine-practice distinction survives empirical scrutiny or collapses under sustained pressure.').

omega_variable(
    identity_lock_mechanism_suppression_internalization,
    'Is the high measured suppression (0.76) structural (external legal and institutional barriers) or internalized (members come to accept the reversal as legitimate even without federal threat)? How much of the suppression persists if the external threat is removed?',
    'Longitudinal tracking of Church practice after federal pressure is fully withdrawn (statehood granted, property threats removed, criminal prosecution ceased). If suppression remains high (members continue to oppose plural marriage out of internalized values), the suppression is partly internalized. If suppression drops sharply when external threat is lifted, suppression is primarily structural.',
    'If internalized: the constraint''s victims are more deeply trapped (they carry the suppression even if the external mechanism is removed, making exit costlier than measured suppression suggests). If structural: the constraint can be dismantled by lifting the external threat, making victims'' identity-lock the binding factor rather than internalized suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression_internalization, empirical, 'Proportion of measured suppression that is structural vs. internalized in the identity-locked membership.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does the exogenous override reading logically foreclose the endogenous reinterpretation reading, or can both be true within a single framework?',
    'Examine whether the core premises contradict: if the Manifesto CAN be both divinely revealed AND federally coerced in the same causal sequence, the readings coexist; if one requires that the other cannot be true, they foreclose. The engine computes the answer via cs_axiom_contradiction; this omega documents the uncertainty.',
    'If they foreclose: only one reading can be true within the Church''s institutional framework, and institutional change will eventually select one. If they coexist: members and leaders can hold both simultaneously (God commanded the reversal AND federal pressure was the mechanism), which is the actual historical situation in much LDS discourse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether exogenous override and endogenous reinterpretation readings are logically compatible or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(marr_tr_t5, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 20, 0.54).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t35, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 35, 0.57).
narrative_ontology:measurement_basis(marr_tr_t35, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement_basis(marr_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(marr_be_t5, observed).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t35, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 35, 0.81).
narrative_ontology:measurement_basis(marr_be_t35, observed).
narrative_ontology:measurement(marr_be_t50, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement_basis(marr_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(marr_su_t5, observed).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t35, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement_basis(marr_su_t35, observed).
narrative_ontology:measurement(marr_su_t50, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement_basis(marr_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The marriage_commitment_legitimacy kernel decomposes into three constraint stories, each instantiating a different reading of what the 1890 Manifesto represents. EXOGENOUS_OVERRIDE_READING (this story) frames it as federal coercion extracting institutional compliance (high extractiveness, snare-flavored). ENDOGENOUS_REINTERPRETATION_READING frames it as genuine prophetic revelation (low-to-moderate extractiveness, rope-flavored coordination). HYBRID_PRAGMATIC_READING frames it as strategic institutional adaptation using prophetic authority to manage exogenous crisis (moderate extractiveness, tangled_rope-flavored). The three stories share the same kernel (the legitimacy of the Manifesto's authority) but diverge in their assessment of what drove the reversal and what structural relationship it instantiates. All three should be present in the corpus; they are linked via network.affects_constraints and are meant to be compared to model the same institutional event under different committer framings. The exogenous override reading influences the sibling readings because the federal pressure is a material fact that any reading must contend with; however, the endogenous reading forecloses the strongest claims of the exogenous reading by asserting divine causation as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__exogenous_override_reading, organized, 0.87).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
