% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading (Pre-Government Liberty)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested Second
 *   Amendment kernel: the individual-right reading that frames the right to
 *   keep and bear arms as a pre-existing individual liberty protected against
 *   federal infringement. This reading rose to institutional dominance
 *   through originalist legal methodology, especially after District of
 *   Columbia v. Heller (2008). The constraint is CLAIMED as a mountain—a
 *   natural constitutional limit on federal authority—while the metrics show
 *   moderate-to-substantial extractiveness and theater. The claim/metric
 *   divergence is the measurement: originalist jurists and individual gun
 *   owners experience this as a structural constitutional fact (emerging
 *   naturally from the text and founding intent); excluded collectives and
 *   collective-right advocates experience the same framing as a constructed
 *   doctrine that benefits identifiable parties (originalists, gun rights
 *   organizations, individual owners) while suppressing alternative readings.
 *   The authored metrics capture this second perspective: the constraint
 *   persists through enforcement of interpretive authority
 *   (suppression_requirement rising as alternative framings are actively
 *   marginalized in law schools and federal courts), and it extracts
 *   constitutional authority in favor of those whose political interests
 *   align with originalism. The measurements run on one shared time grid: the
 *   constraint's extractiveness has risen from the pre-Heller period (0.42,
 *   projected) through Heller's aftermath and into the contemporary period
 *   (0.58, observed; trending slightly downward to 0.58 at t=35, projected,
 *   due to state-level regulatory assertiveness pushing back). Theater has
 *   risen as institutional defense of the reading has intensified despite
 *   persistent scholarly challenge.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Direct beneficiaries of the reading; gain constitutional protection from federal prohibition
 *   - originalist_jurists_and_scholars: Agenda-setters; control institutional interpretation and defend the reading's legitimacy
 *   - federal_regulatory_authority: Constrained payer; finds their regulatory scope narrowed by this reading's limit on federal power
 *   - collective_right_advocates: Structurally excluded; would contest the reading but lack institutional authority to instantiate alternatives
 *   - state_governments: Partial beneficiaries; retain regulatory autonomy within the constraint's boundaries
 *   - civic_republican_interpreters: Structurally excluded; occupy a middle ground between individual and militia framings, marginalized by binary institutional choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.47).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment Individual Right Reading (Pre-Government Liberty)").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional/political").

domain_priors:emerges_naturally(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, '22f126b7-878e-490d-b045-855b1c604b58').
narrative_ontology:cs_kernel_codification('22f126b7-878e-490d-b045-855b1c604b58', fixed_text).
narrative_ontology:cs_authority_grounding('22f126b7-878e-490d-b045-855b1c604b58', lineage).
narrative_ontology:cs_interpretation_layer_present('22f126b7-878e-490d-b045-855b1c604b58').
narrative_ontology:cs_reading_relation('22f126b7-878e-490d-b045-855b1c604b58', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('22f126b7-878e-490d-b045-855b1c604b58', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('22f126b7-878e-490d-b045-855b1c604b58', foundational, natural_right_doctrine).
narrative_ontology:cs_axiom_status(natural_right_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('22f126b7-878e-490d-b045-855b1c604b58', natural_right_doctrine, deontological).
narrative_ontology:cs_axiom('22f126b7-878e-490d-b045-855b1c604b58', secondary, originalist_interpretation_as_legitimate).
narrative_ontology:cs_axiom_status(originalist_interpretation_as_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('22f126b7-878e-490d-b045-855b1c604b58', originalist_interpretation_as_legitimate, conventional).
narrative_ontology:cs_reference_frame('22f126b7-878e-490d-b045-855b1c604b58', founding_intent_preserves_individual_liberty).
narrative_ontology:cs_drift_state('22f126b7-878e-490d-b045-855b1c604b58', contemporary_regulatory_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('22f126b7-878e-490d-b045-855b1c604b58', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, originalist_constitutional_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, natural_right_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, originalist_interpretation_methodology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, individual gun owners hold a constitutional liberty pre-existing government. They benefit from the interpretive framing that places their claim to firearm ownership outside the scope of legitimate federal regulation. Their practical exit from the constraint is constrained: they remain subject to state and local law, but the federal government's regulatory authority is curtailed by this reading's understanding of the constitutional text. They organize politically to defend this reading against revisionist interpretations.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, constrained, national).

% Under this reading, federal law-making authority is constrained by a pre-existing individual right. Federal agencies and Congress operate within a narrowed scope: they cannot enact blanket prohibitions or regulations that effectively eliminate the individual right to keep and bear arms. This constrains their policy options and requires justification for restrictions. They experience this reading as a barrier to regulatory flexibility; their exit would require constitutional amendment or overturning the reading itself.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority, payer,
    institutional, generational, constrained, national).

% Interpret and defend this reading through legal scholarship, judicial opinions, and constitutional argument. They set the interpretive terms by which the text is understood. They have the highest exit optionality: they can shift interpretive approaches, adopt alternative methodologies, or reframe the founding intent. They collect authority and professional legitimacy from maintaining this reading as law.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, originalist_jurists_and_scholars, agenda_setter,
    institutional, generational, arbitrage, national).

% Argue for the militia-dependent reading of the Second Amendment but are excluded from the dominant interpretive frame by institutional decisions (especially after District of Columbia v. Heller, 2008). They remain structurally outside the conversation that this reading dominates, though they continue to argue within academic and some judicial circles. Their exclusion is enforced by appellate precedent and the institutional weight of originalist interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, collective_right_advocates, excluded,
    organized, generational, trapped, national).

% Advocate for a reading that centers armed citizenship and republican self-governance as the right's proper frame. This reading sits between the individual-right and militia-dependent poles. They are structurally excluded from the dominant binary because institutional judicial power and originalist interpretive hegemony marginalizes their framing; they appear in scholarship and dissents but lack the decision-making authority to instantiate their reading as law.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, civic_republican_interpreters, excluded,
    organized, generational, trapped, national).

% Under this reading's scope, state governments retain substantial regulatory authority over firearms (the constraint does not preempt state law). They benefit from the federal-level constraint because it frees them from federal displacement of their own regulatory choices, and they can still enact robust firearms regulations within their jurisdictions. Their exit options are mobile: they can lobby for constitutional amendment or support revisionist readings, but they also have policy space to regulate within the constraint's boundaries.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_governments, beneficiary,
    institutional, generational, mobile, national).

% Examines the constraint structure: how this reading instantiates and enforces itself, what its beneficiaries and excluded voices are, and how its classification diverges across seats. The observer notes that what appears as a natural constitutional law to originalist jurists appears as a constructed legal doctrine to those excluded by it.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared interpretive framework for understanding the constitutional text: originalist jurists, individual gun owners, and certain state governments coordinate around the reading that the Second Amendment protects an individual pre-political right. This coordinates how the Constitution is understood and what range of federal regulation is permissible.
% TRANSFER_FUNCTION: Transfers constitutional authority from living-constitution and alternative hermeneutical approaches toward originalism and the individual-right doctrine. Practically, it moves the scope of legitimate federal firearms regulation downward (constrains federal power) and protects individual gun owners from certain federal prohibitions. It transfers interpretive legitimacy to originalist methodology.
% ABSENT_VOICES: Collective-right scholars and civic-republican interpreters who argue the militia clause is binding or that armed citizenship (not bare individual ownership) is the right's proper frame. They would dispute the originalist reconstruction of founding intent, citing different historical sources and a different reading of militia-era documents. They are excluded from dominant institutional interpretation (federal courts, elite law schools) but persist in scholarship and dissent.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and federal authority expanded to permit blanket firearms prohibitions, the constitutional landscape would reorganize: individual gun owners would lose federal-level protection, federal regulatory scope would expand, and an alternative reading (collective-right or civic-republican) would become institutionally dominant. The entire doctrinal structure of Second Amendment jurisprudence would require reconstruction.
% FOUNDING_PROBLEM: The founding problem this reading claims to solve: the framers sought to protect a pre-political individual right to keep and bear arms against governmental overreach. The right was understood as pre-existing the Constitution, and the Amendment was declaratory of this antecedent liberty, binding federal authority to respect it.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and Supreme Court majorities (Heller opinion, recent appellate decisions) attest that founding intent protected an individual right. Collective-right and civic-republican scholars (Saul Cornell, Michael Waldman, Patrick Charles, Cary Rosen) cite founding documents, state constitutions, and militia laws to argue the individual-right reading misrepresents founding intent. Historians outside the originalist school predominantly support militia-dependent or civic-republican readings, though some originalist historians defend the individual-right reading. Legislative history is contested: different sources weigh toward different conclusions depending on which documents are selected. No external independent arbiter has resolved the contest; academic scholarship remains divided along interpretive lines, with institutional power currently favoring originalists in federal courts.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_arms_right__individual_right_reading),
    narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.58 at t=35) captures the constraint's effect on the distribution of constitutional authority: it privileges originalism as the valid interpretive method and the individual right as the correct reading. This is extracted from collective-right scholars, living-constitutionalist judges, and alternative hermeneutical frameworks that are systematically disadvantaged in law schools and appellate courts. The suppression metric (0.47) reflects that this extraction requires active institutional defense: alternative readings are not naturally impossible (they persist in scholarship and dissent), so their exclusion from dominance requires enforcement through hiring, curriculum, and precedent. The theater ratio (0.29) is moderate: the originalist methodology is genuinely applied to other constitutional texts (not pure theater), but a growing share of the work in Second Amendment jurisprudence is defending the individual-right reading against accumulated scholarly challenge rather than substantively reinterpreting the amendment itself. The accessibility_collapse metric (0.72) is high because once the individual-right reading is established as law, alternative readings (collective-right, civic-republican) become legally foreclosed at the federal level—they cannot be adopted by federal courts without overturning precedent. But they remain intellectually and academically accessible (not 0.9), so collapse is substantial but not complete. The resistance metric (0.68) is high because substantial, organized scholarship, state regulatory initiatives, and social movements resist this reading's implications and claim; the constraint is not passively accepted but actively contested. The claim/metric independence is deliberate: the claim is that this constraint is a mountain (natural constitutional fact). The metrics describe it as substantially extractive, requiring active suppression of alternatives, and meeting substantial resistance—the data a false-summit detector would want to see. The engine computes the type from these metrics; where computed type diverges from claimed type, that divergence is exactly what the corpus measures.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist jurist's seat, this constraint is natural constitutional law: the text and founding intent unambiguously protect an individual right; the Supreme Court correctly vindicated this in Heller; alternative readings are historically inaccurate. From the individual gun owner's seat, this constraint is a liberty—a recognition of something that was always theirs. From the federal regulatory authority's seat, this constraint is an obstacle: it prevents reasonable public-health measures and constrains democratic choice. From the collective-right advocate's seat, this constraint is a constructed doctrine that misreads history, serves specific contemporary interests (gun rights organizations), and suppresses more historically grounded framings. From the civic-republican interpreter's seat, this constraint is a false binary that ignores the militia-citizenship nexus and disserves the founding's genuine concern with republican self-governance. The engine computes different directionalities from these seats: originating judges, gun owners, and those who benefit from originalism as interpretive power sit near the beneficiary end (d low). Federal regulators sit near the target end (constrained, bearing the cost of being denied regulatory scope). Collective-right advocates sit at an extreme target end (trapped, excluded, their interpretive voice suppressed). The analytical observer sits at d=0.5, neutral.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declaration (individual_gun_owners, originalist_constitutional_interpreters) derives d values near 0.0-0.2 (beneficiaries, low extraction felt): gun owners collect constitutional protection; originalists collect interpretive authority and professional legitimacy. The federal regulatory authority derives high d (0.75-0.85, near target end): constrained scope, constrained exit (must amend constitution or overturn reading to change it). Collective-right advocates are locked at extreme high d (0.95): identity_locked to a defeated scholarly position, trapped in institutional exclusion, bearing the cost of judicial precedent against them. State governments derive moderate d (0.45-0.55): they retain regulatory autonomy within the constraint's scope, so they are neither pure beneficiaries nor targets—they benefit from federal-level constraint while retaining their own authority. No directionality_overrides are needed: the structural derivation from beneficiary/victim declarations + exit options produces coherent d values across seats. The constraint's power atom is institutional (Supreme Court, law schools, originalist networks), and scope is national; the engine scales effective extraction upward for targets (federal regulators) and downward for beneficiaries (gun owners), producing a wide per-seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint carries a live founding problem (preserve pre-existing individual liberty against federal overreach) whose status is contested: originalists attest it is still live (federal overreach is an ongoing threat), while collective-right scholars attest it is dead (the founding problem was militia-organization, not individual ownership, and that problem was solved in 1791). The constraint's disappearance_verdict is world_rearranges: federal firearms law would fundamentally reorganize if this reading were overturned. The mismatch (contested founding problem status + world_rearranges verdict) does not trigger mandatrophy: mandatrophy would require status=dead (the problem is gone) AND world_rearranges (the constraint would reorganize the world if removed), which would indicate zombie-constraint persistence. Here, status is contested, so mandatrophy firing is not warranted. The constraint is not a vestigial function; it is an actively-defended interpretive reading in institutional contest. However, the rise in theater_ratio over the interval (0.18 to 0.30) and the substantial resistance metric (0.68) suggest the constraint's functional claims (that it protects founding intent, that it emerges naturally from the text) are increasingly separated from the constraint's actual operation (defending originalist authority against scholarly challenge). This is not mandatrophy but it is a signal of institutional strain: if the theater ratio continues to rise and resistance peaks, the reading may transition to piton status (maintained by institutional inertia rather than genuine functional fit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the individual right to keep and bear arms a pre-political natural liberty, or is it a constructed legal doctrine that serves contemporary political interests and benefits identifiable institutional and individual actors?',
    'Historical scholarship examining founding intent, comparing competing interpretations of the Amendment''s text against a broader corpus of founding documents, militia laws, and state constitutions. Empirical examination of whether the framing enables selective contemporary policy (gun rights) while suppressing alternative readings (collective-right, civic-republican). Institutional analysis of how originalism became dominant in law schools and federal courts despite persistent scholarly dissent.',
    'If the right is natural law, the constraint is a mountain: it emerges from constitutional text and founding intent, neutral among parties, constraining federal authority as a brute fact. If it is constructed doctrine, the constraint is a tangled_rope or snare: it benefits originalist jurists, gun rights organizations, and individual owners while suppressing collective-right and civic-republican scholarship; its persistence depends on institutional enforcement, not natural inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether this constraint is a discovered natural constitutional principle or a constructed interpretive doctrine serving specific interests.').

omega_variable(
    militia_clause_interpretive_necessity,
    'Is the ''well regulated Militia'' clause a binding constraint on the right (militia-dependent reading), a prefatory statement of motivation (individual-right reading), or the proper context for understanding armed citizenship (civic-republican reading)?',
    'Linguistic analysis of 18th-century grammar and punctuation conventions; comparative study of how militia clauses were used in state constitutions; examination of founding documents discussing the militia; scholarly consensus from outside originalist and gun-rights advocacy circles.',
    'If the militia clause is binding, the collective-right reading becomes more defensible and the individual-right reading must be substantially narrowed (perhaps to militia members only). If the clause is merely prefatory, the individual-right reading stands stronger. If the clause signals a civic-republican context (armed citizenship as prerequisite for self-governance), the civic-republican reading becomes central and both pure-individual and pure-collective readings are partially foreclosed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militia_clause_interpretive_necessity, empirical, 'Whether the militia clause is a binding limitation or merely a prefatory statement of rationale.').

omega_variable(
    suppression_mechanism_institutional_vs_epistemic,
    'Is the suppression of collective-right and civic-republican readings a result of institutional power (originalists controlling law schools and federal courts) or epistemic validity (the individual-right reading is genuinely more defensible)? Or both?',
    'Comparative analysis of law school curriculum and hiring in originalist vs. non-originalist programs; study of how Supreme Court precedent forecloses alternative readings at the appellate level; examination of whether the marginalizing of alternative readings occurs through counter-argument and evidence, or through institutional gatekeeping and citation suppression.',
    'If suppression is primarily institutional, the constraint is a snare with enforced exclusion of dissenting voices. If primarily epistemic, the suppression is a rational sorting of views by validity. If mixed, the constraint is a tangled_rope with both genuine interpretive dominance and institutional enforcement of that dominance. A finding of primarily institutional suppression would support reclassification away from mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_vs_epistemic, empirical, 'Whether the dominance of the individual-right reading is epistemic, institutional, or both.').

omega_variable(
    founding_problem_survival_vs_obsolescence,
    'Does the founding problem this reading claims to solve (protect individual liberty against federal overreach in firearms) remain live, or has it been solved and the constraint now persists through institutional inertia?',
    'Examination of federal policy and court decisions: are new federal infringements on the individual right a genuine threat, or have federal actors largely accepted the constraint? Survey of individual gun owners and regulators about whether they perceive ongoing threat. Comparison of federal firearms regulation across time: is federal scope expanding despite the reading, or is the reading effectively constraining federal power?',
    'If the founding problem is genuinely live, the constraint may be classified as a rope or tangled_rope solving a real coordination/constraint problem. If the problem is dead but the constraint persists, the constraint approaches piton status (institutional maintenance without functional necessity). A finding of dead founding problem + world_rearranges verdict would trigger mandatrophy flags.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_survival_vs_obsolescence, empirical, 'Whether the constraint''s founding problem remains live or has been solved.').

omega_variable(
    false_summit_candidate_beneficiary_capture,
    'Does this constraint, claimed as a natural mountain, actually benefit identifiable institutional and individual parties (originalists, gun rights organizations, individual gun owners) in a way that suggests it is a constructed doctrine rather than a natural constitutional fact?',
    'Mapping of who benefits (originalist legal scholars gain professional authority and career advancement; gun rights organizations gain constitutional footing for their policy; individual gun owners gain protection from federal prohibition). Comparison with alternative readings to examine whether they would distribute benefits/costs differently. Examination of whether the beneficiaries have political power to sustain the reading despite scholarly challenge (institutional enforcement signature).',
    'Evidence of identifiable beneficiaries, combined with high suppression_requirement and rising theater_ratio, would trigger false-summit detection and recommend reclassification from mountain toward tangled_rope or snare. This is the FSM omega: the natural-law claim paired with observable beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_candidate_beneficiary_capture, empirical, 'Whether identifiable parties benefit from this reading being treated as natural law, suggesting false-summit status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__individual_right_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(seco_tr_t0, projected).
narrative_ontology:measurement(seco_tr_t7, second_amendment_arms_right__individual_right_reading, theater_ratio, 7, 0.21).
narrative_ontology:measurement_basis(seco_tr_t7, projected).
narrative_ontology:measurement(seco_tr_t14, second_amendment_arms_right__individual_right_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement_basis(seco_tr_t14, observed).
narrative_ontology:measurement(seco_tr_t21, second_amendment_arms_right__individual_right_reading, theater_ratio, 21, 0.28).
narrative_ontology:measurement_basis(seco_tr_t21, observed).
narrative_ontology:measurement(seco_tr_t28, second_amendment_arms_right__individual_right_reading, theater_ratio, 28, 0.29).
narrative_ontology:measurement_basis(seco_tr_t28, observed).
narrative_ontology:measurement(seco_tr_t35, second_amendment_arms_right__individual_right_reading, theater_ratio, 35, 0.3).
narrative_ontology:measurement_basis(seco_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__individual_right_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(seco_be_t0, projected).
narrative_ontology:measurement(seco_be_t7, second_amendment_arms_right__individual_right_reading, base_extractiveness, 7, 0.48).
narrative_ontology:measurement_basis(seco_be_t7, projected).
narrative_ontology:measurement(seco_be_t14, second_amendment_arms_right__individual_right_reading, base_extractiveness, 14, 0.54).
narrative_ontology:measurement_basis(seco_be_t14, observed).
narrative_ontology:measurement(seco_be_t21, second_amendment_arms_right__individual_right_reading, base_extractiveness, 21, 0.58).
narrative_ontology:measurement_basis(seco_be_t21, observed).
narrative_ontology:measurement(seco_be_t28, second_amendment_arms_right__individual_right_reading, base_extractiveness, 28, 0.6).
narrative_ontology:measurement_basis(seco_be_t28, observed).
narrative_ontology:measurement(seco_be_t35, second_amendment_arms_right__individual_right_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement_basis(seco_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__individual_right_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(seco_su_t0, projected).
narrative_ontology:measurement(seco_su_t7, second_amendment_arms_right__individual_right_reading, suppression_requirement, 7, 0.39).
narrative_ontology:measurement_basis(seco_su_t7, projected).
narrative_ontology:measurement(seco_su_t14, second_amendment_arms_right__individual_right_reading, suppression_requirement, 14, 0.43).
narrative_ontology:measurement_basis(seco_su_t14, observed).
narrative_ontology:measurement(seco_su_t21, second_amendment_arms_right__individual_right_reading, suppression_requirement, 21, 0.46).
narrative_ontology:measurement_basis(seco_su_t21, observed).
narrative_ontology:measurement(seco_su_t28, second_amendment_arms_right__individual_right_reading, suppression_requirement, 28, 0.48).
narrative_ontology:measurement_basis(seco_su_t28, observed).
narrative_ontology:measurement(seco_su_t35, second_amendment_arms_right__individual_right_reading, suppression_requirement, 35, 0.47).
narrative_ontology:measurement_basis(seco_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, federal_firearms_regulation_authority).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, individual_self_defense_entitlement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the second_amendment_arms_right kernel. The three readings (individual_right, collective_right, civic_republican) are structurally distinct constraints with different ε values, beneficiary sets, and classification trajectories. The individual_right_reading instantiated here (ε=0.58, claimed_type=mountain) directly influences and coexists with the collective_right_reading (ε likely higher, claimed_type likely snare or tangled_rope, as it emphasizes state regulatory authority over individual owner protection) and civic_republican_reading (ε moderate, claimed_type likely rope, as it attempts coordination between individual and collective concerns). All three share the same fixed_text kernel and are linked via network.affects_constraints for contamination analysis. Do NOT merge these readings into one constraint with multiple observables or measurement bases—each is a coherent constraint story under its own reading, with its own stakeholder configuration. The divergence between readings is the measurement the corpus takes; the three stories together show how one constitutional kernel can instantiate multiple ε-invariant constraints depending on interpretive framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__individual_right_reading, organized, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
