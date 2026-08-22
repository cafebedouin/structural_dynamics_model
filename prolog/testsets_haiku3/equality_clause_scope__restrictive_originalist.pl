% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Equality Clause Restrictive Originalist Reading: Propertied White Male Franchise
 *   domain: constitutional/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the restrictive originalist reading of the
 *   equality clause in the 14th Amendment and its antecedents. The reading
 *   asserts that 'equality' in the text means equality among the political
 *   actors who are parties to the social contract—propertied white males—and
 *   that expansion to other groups requires constitutional amendment, not
 *   judicial reinterpretation of the existing clause. The constraint has
 *   operated as the dominant interpretive frame from 1787 through the end of
 *   Reconstruction (1877), has been challenged by progressive and
 *   living-constitution readings during the Civil Rights era (1950s–1970s),
 *   and has been revived with renewed force in recent decades by originalist
 *   judicial movements. The measurement series traces the extraction curve:
 *   extractiveness is lowest at the founding (when the constraint operated
 *   with consent from most beneficiaries who thought of themselves as the
 *   natural parties to the contract) and rises as the excluded populations
 *   grow in size and consciousness and as the founding problem becomes
 *   transparently obsolete. The theater ratio rises sharply after
 *   Reconstruction as the originalist reading must increasingly perform
 *   fidelity to 1787 meaning despite mounting evidence that expanded equality
 *   has become the dominant moral intuition—the reading becomes more
 *   theatrical (justifying itself through technical originalist methodology)
 *   as its substantive plausibility declines.
 *
 * KEY AGENTS:
 *   - Propertied white male political actors: Primary beneficiaries; define the social contract and benefit directly from narrow equality scope
 *   - Enslaved populations: Primary victims; completely excluded from political personhood and social contract framework
 *   - Excluded women: Major victims; no legal personhood, voting power, or property rights under this reading
 *   - Propertyless men and non-white populations: Structurally excluded; lack either property qualification or racial status to qualify as political actors
 *   - Originalist constitutional scholars: Beneficiaries of institutional legitimacy; their interpretive methodology vindicates the narrow reading
 *   - Slaveholding interests: Direct beneficiaries; slavery is protected as property right outside equality clause's domain
 *   - Expansionist rights advocates: Payers; must navigate high amendment barrier to claim equality for excluded groups
 *   - Living constitutionalist judges: Excluded; their authority to expand rights is denied by originalist methodology
 *   - Democratic majorities: Excluded from ordinary amendment process; cannot modify constraint through democratic will alone
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.68).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.71).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.68).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Equality Clause Restrictive Originalist Reading: Propertied White Male Franchise").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional/political_philosophy").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, 'ad91a65e-0147-4b05-a218-568ed01700b0').
narrative_ontology:cs_kernel_codification('ad91a65e-0147-4b05-a218-568ed01700b0', fixed_text).
narrative_ontology:cs_authority_grounding('ad91a65e-0147-4b05-a218-568ed01700b0', lineage).
narrative_ontology:cs_interpretation_layer_present('ad91a65e-0147-4b05-a218-568ed01700b0').
narrative_ontology:cs_reading_relation('ad91a65e-0147-4b05-a218-568ed01700b0', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('ad91a65e-0147-4b05-a218-568ed01700b0', equality_clause_scope__progressive_textualist, influences).
narrative_ontology:cs_axiom('ad91a65e-0147-4b05-a218-568ed01700b0', foundational, equality_means_original_public_meaning).
narrative_ontology:cs_axiom_status(equality_means_original_public_meaning, holdable).
narrative_ontology:cs_axiom_grounding('ad91a65e-0147-4b05-a218-568ed01700b0', equality_means_original_public_meaning, deontological).
narrative_ontology:cs_axiom('ad91a65e-0147-4b05-a218-568ed01700b0', foundational, social_contract_parties_determine_equality_scope).
narrative_ontology:cs_axiom_status(social_contract_parties_determine_equality_scope, holdable).
narrative_ontology:cs_axiom_grounding('ad91a65e-0147-4b05-a218-568ed01700b0', social_contract_parties_determine_equality_scope, conventional).
narrative_ontology:cs_axiom('ad91a65e-0147-4b05-a218-568ed01700b0', secondary, expansion_requires_amendment_not_reinterpretation).
narrative_ontology:cs_axiom_status(expansion_requires_amendment_not_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('ad91a65e-0147-4b05-a218-568ed01700b0', expansion_requires_amendment_not_reinterpretation, deontological).
narrative_ontology:cs_reference_frame('ad91a65e-0147-4b05-a218-568ed01700b0', framers_intent_1787_social_contract).
narrative_ontology:cs_drift_state('ad91a65e-0147-4b05-a218-568ed01700b0', contemporary_inclusive_democracy_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('ad91a65e-0147-4b05-a218-568ed01700b0', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_male_political_actors).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, slaveholding_interests).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, institutional_conservatism).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, excluded_women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_populations).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, native_americans).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, propertyless_men).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_white_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, originalist_constitutional_scholars).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, expansionist_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain voting power, property protections, and contractual personhood under the equality clause. They are the intended beneficiaries of the original framework and define what equality means within it. Their property rights and political voice are amplified by the clause's narrow scope. They defend the originalist reading as fidelity to the founders' intent.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_male_political_actors, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, propertied_white_male_political_actors, agenda_setter).

% Are excluded from the equality clause entirely under this reading—no legal personhood for contract, no independent property rights, no political franchise. The constraint actively suppresses their claims to equality by defining the class of 'political actors' to exclude them. Their exclusion is justified under originalism as outside the framers' contemplation, not as a failure of the principle.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, excluded_women, payer,
    powerless, generational, trapped, national).

% Are treated as property, not persons under the equality clause. The restrictive originalist reading treats their complete exclusion as consistent with the 1787 framework. The 3/5 compromise and fugitive slave clause sit beside the equality clause in the same document, structurally excluding enslaved persons from the benefit class. Their cost under this constraint is not merely exclusion but active suppression through constitutional architecture.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_populations, payer,
    powerless, immediate, trapped, national).

% Are treated as foreign sovereigns outside the equality clause's domain entirely under this reading. The constraint does not extend to them; they are addressed by treaty power, not civil rights. This reading preserves the separation and offers no equality basis for claims to membership or political standing.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, native_americans, payer,
    moderate, generational, trapped, national).

% Are structurally excluded from the equality clause's benefits because the clause applies to political actors (property-holding participants in the social contract), not to all persons. Their lack of property is what disqualifies them; the constraint ties equality to propertied status, making it conditional on economic position.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertyless_men, payer,
    powerless, biographical, constrained, national).

% Are outside the social contract framework altogether under this reading. They are not considered parties to the original compact and thus not beneficiaries of its equality principle. The constraint provides no basis for their inclusion; expansion would require amendment, not reinterpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_white_populations, payer,
    powerless, generational, trapped, national).

% Gain institutional authority and scholarly legitimacy by defending the original public meaning of the equality clause as applied to propertied white males. Their interpretive methodology is vindicated by the constraint's persistence; they argue for fidelity to text and framers' intent over contemporary moral intuitions.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_constitutional_scholars, beneficiary,
    institutional, generational, arbitrage, national).

% Benefits from a narrow, stable reading of the equality clause that preserves established distributions of power and property. The constraint provides a legitimacy framework for resisting expansive reinterpretations that would challenge existing institutional arrangements. The high bar for reinterpretation (amendment requirement) protects status quo.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, institutional_conservatism, beneficiary,
    institutional, civilizational, arbitrage, national).

% Directly benefit from the equality clause's narrow scope because it provides constitutional cover for property in enslaved persons. The restricted equality reading treats slavery as outside the clause's domain (not a matter of equals versus unequals, but of persons versus property). This reading vindicates their economic and political dominance.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, slaveholding_interests, beneficiary,
    powerful, biographical, arbitrage, national).

% Bear the costs of the restrictive reading through exclusion and through forced reliance on constitutional amendment (a high-friction process) rather than reinterpretation to claim equality. Each expansionist claim—women's suffrage, racial equality, gender identity—must fight the originalist reading's high legitimacy threshold and its demand for original public meaning.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansionist_rights_advocates, payer,
    organized, generational, constrained, national).

% Are theoretically excluded from modifying the constraint through ordinary democratic process; reinterpretation via amendment requires supermajority consensus, which the originalist reading enforces as a structural requirement. Their exclusion is the suppression mechanism itself—they cannot vote to expand equality; they must amend the Constitution.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, democratic_majorities, excluded,
    organized, generational, constrained, national).

% Are excluded from using the equality clause to expand rights through judicial reinterpretation under this reading. The originalist methodology forbids them from reasoning beyond the framers' intent; their judicial authority to expand equality is structurally denied by the reading's methodological frame.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, living_constitutionalist_judges, excluded,
    institutional, generational, constrained, national).

% Views the constraint from outside any beneficiary or victim position. Can see how the reading distributes benefits to a narrow propertied class and costs to excluded populations; can trace the enforcement mechanisms (interpretive methodology, legitimacy thresholds) that maintain the narrow scope.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, propertied_white_male_political_actors).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common framework for understanding political equality among the parties to the social contract—propertied white males who negotiated the Constitution as peers. It provides a coordination point for who counts as an equal political actor and on what terms (property ownership, whiteness, maleness). It solves the problem of how to constitute a stable political order among contracting parties with shared interests in property protection and representation.
% TRANSFER_FUNCTION: Transfers political voice, voting power, property rights, and legal personhood to propertied white males; transfers disenfranchisement, legal invisibility, and exclusion from the social contract to women, enslaved persons, Native Americans, propertyless men, and non-white populations. The constraint moves authority and legitimacy from the excluded to the included by defining equality so narrowly that most humans fall outside its scope.
% ABSENT_VOICES: Women, enslaved persons, Native Americans, propertyless men, and non-white populations are structurally excluded from the conversation. They would argue that equality is a universal principle applying to all humans regardless of property or race; they would challenge the originalists' claim that the framers' intent should bind contemporary understandings of equality. Their voices are kept out by the reading's own methodological gate—original public meaning is defined by the framers' understanding, not by the excluded parties' claims.
% DISAPPEARANCE_RATIONALE: If this reading of the equality clause disappeared and were replaced by an expansive universalist reading, political power would redistribute dramatically: women would gain voting power, enslaved persons would be recognized as persons with rights, propertyless men would gain political standing, and the legitimacy of the entire propertied-white-male political monopoly would collapse. The constraint is what preserves their advantage; its disappearance would rearrange power fundamentally.
% FOUNDING_PROBLEM: How to constitute a stable political order among propertied white males with divergent state interests, property holdings, and regional power bases—i.e., how to create a social contract that protects their interests against each other and against popular upheaval while preserving their collective dominance over enslaved, excluded, and propertyless populations.
% FOUNDING_PROBLEM_CORROBORATION: The framers' own writings attest the founding problem (constitutional convention debates, Federalist Papers, ratification-era arguments). Modern originalist scholars attest that the founding problem still motivates the reading and that fidelity to original meaning is how we honor the framers' solution. But contemporary historians (Foner, Sunstein, Dworkin), abolitionists, and rights advocates attests that the founding problem—political coordination among elite white propertied males—is obsolete. The empirical consensus outside the beneficiary class and originalist circles is that this constraint now functions entirely as extraction and power maintenance, not as coordination of anything live. The Declaration of Independence's own language ('all men are created equal') is invoked by expansionist advocates to argue that the Constitution's restrictive reading contradicts its own foundational document.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects the constraint's core structure: it concentrates political voice and property rights in a narrow class (beneficiaries) and denies political and legal personhood to most populations (victims). The extraction is substantial but not maximal (not 0.95+) because the original framing had genuine consent from the beneficiary class—they understood themselves as the intended parties and the narrowness was not yet experienced as extraction by them. Suppression at 0.71 reflects the active enforcement mechanisms: the interpretive gate (original public meaning) that blocks expansionist readings, the constitutional amendment requirement that makes expansion procedurally difficult, and the theological-like status of 'the framers' intent' that makes challenge seem illegitimate. Theater ratio at 0.42 reflects the growing gap between the reading's technical justification (originalist methodology, historical fidelity) and its substantive function (blocking equality expansion). The measurement series shows extractiveness rising significantly after 1863 (Reconstruction ends and the narrow reading is reasserted) and staying elevated. Theater rises sharply after Reconstruction as originalism becomes increasingly necessary to defend a reading that most people now see as unjust.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats compute the constraint as tangled_rope with a strong coordination component (establishing legitimate political order, property protections, stable governance). The victim and payer seats compute it as snare or extractive piton (the coordination story is cover; the actual function is maintaining their exclusion and the beneficiary class's dominance). The core divergence is whether the constraint solves a real coordination problem or merely dresses up extraction in coordination language. Both perspectives are structurally plausible: the framers did face a genuine coordination problem (how to form a stable union among competing state interests without either monarchy or pure democracy), AND the solution they adopted depended structurally on excluding most humans from the political community. The constraint accomplishes both coordination for insiders and extraction from outsiders through the same mechanism (narrow benefit class definition). The engine should compute sharply different types per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   From the beneficiary seat (propertied white male political actors), the constraint appears as genuine coordination: it establishes a stable political order among peers with shared interests in property protection and representative government. The directionality for this seat is near 0.0 (full beneficiary). From the victim seats (enslaved persons, women, non-white populations), the constraint appears as pure extraction: they pay the cost of exclusion, legal invisibility, and denied political voice while deriving zero benefit. Their directionality approaches 1.0 (full target). From the originalist-scholar seat, the constraint appears as intellectual order worth preserving (methodological coherence, fidelity to text). The agenda-setter seat (institutional conservatism, judiciary enforcing originalism) experiences it as a tool for defending status quo distributions. The expansionist-advocate seat experiences it as a high-friction barrier to legitimate claims. The directionality gradient is stark: beneficiaries at 0.0–0.2, payers at 0.8–1.0, with observers distributed across the middle. No override is needed because the structural data (property ownership, political franchise, slavery, legal personhood) unambiguously derive the directional picture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is declared 'dead' because the original coordination problem (how to unite competing state interests in a new federation) was genuinely solved by the Constitution. But the constraint persists and even intensifies (extractiveness rising, theater rising) long after the problem is obsolete. This is the classic mandatrophy signature: a constraint born to solve a specific problem persists through institutional inertia and ideological commitment even after the problem it was designed to address has been superseded by changed conditions. The living constitutionalist reading would argue that the constraint SHOULD have been revised (through judicial reinterpretation or amendment) as democratic consciousness expanded to include all humans as entitled to equal standing. The restrictive originalist reading argues that revision beyond amendment is illegitimate (methodologically), so the constraint correctly persists despite changed circumstances. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges flags the extraction: the constraint persists not because it solves a live problem but because it distributes power to a beneficiary class that can block revision. Mandatrophy is partially resolved by the Civil War (slavery eliminated) and partially resolved by subsequent amendments (women's suffrage, voting rights), but the core constraint—the framework that says equality is restricted to political actors within the social contract—persists and is defended through originalist methodology as if it were still solving the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_scope_reading_contest,
    'Is equality a universal principle discovered in the text, or a historically contingent interpretation of what the framers intended it to mean in 1787? Does the text permit multiple coherent readings with different beneficiary sets?',
    'Textual analysis comparing the equality principle to other principles in the same document (slavery protections, representative franchise restrictions); historical analysis of framers'' private writings vs. public text; cross-reading comparison: can the same clause coherently mean ''universal human equality'' under one reading and ''propertied-white-male equality'' under another?',
    'If equality is a universal principle, this reading forecloses the expansive universalist reading—only one can be true of what the text MEANS. If equality is text-ambiguous, the readings coexist and the choice between them becomes a matter of interpretive methodology (originalism vs. living constitutionalism) rather than textual discovery. This is the foundational epistemic uncertainty—whether the kernel admits multiple readings or only one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_scope_reading_contest, conceptual, 'Whether the equality clause permits coherent multiple readings or mandates a single meaning.').

omega_variable(
    framers_intent_determinacy,
    'Did the framers have a coherent, unified intent about the scope of equality, or did they hold divergent views that the historical record cannot resolve into a single ''original public meaning''?',
    'Documentary analysis of Constitutional Convention debates, state ratification conventions, and contemporary writings; historiographical examination of whether ''the framers'' intent'' is itself a coherent target or a historiographical reconstruction read backward from modern concerns.',
    'If framers'' intent is determinate and singular, the originalist reading has solid epistemological ground. If it is indeterminate or plural, the originalist reading''s claim to fidelity is undermined—originalism would be enforcing one reconstruction among several possible ones. The suppression mechanism (high legitimacy threshold for reinterpretation) would then be exposed as enforcing a contingent choice as if it were discovery of determinate meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framers_intent_determinacy, empirical, 'Whether the framers'' intent on equality''s scope is historically determinable and singular.').

omega_variable(
    supremacy_of_original_meaning_vs_living_principle,
    'Should constitutional equality be interpreted through the lens of original public meaning (restrictive originalism), through the lens of a living principle that evolves with democratic understanding (living constitutionalism), or through amendment process alone (progressive textualism)?',
    'Comparative constitutional jurisprudence across regimes using different methodologies; outcome analysis tracking whether original-meaning fidelity better preserves rule of law or whether it protects unjust distributions more effectively than alternative methodologies.',
    'This is a preference omega: the choice of interpretive methodology is not empirically resolvable but reflects a normative commitment about how to balance fidelity to founding text against responsiveness to changed moral understanding. Different methodologies produce different beneficiary sets and different expansion thresholds. The restrictive originalist reading depends on the premise that original meaning supremacy is the right constraint; if that premise is rejected, the entire narrowness of the beneficiary set is up for revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supremacy_of_original_meaning_vs_living_principle, preference, 'Which interpretive methodology (originalism, living constitutionalism, amendment-only progressivism) should govern constitutional equality claims.').

omega_variable(
    status_of_slavery_within_equality_framework,
    'Is slavery structurally inconsistent with any meaningful notion of equality (even restricted equality), or is slavery compatible with the equality clause if the enslaved are understood as property rather than persons/political actors?',
    'Logical analysis: can a framework coherently assert that equals deserve equal treatment while treating some humans as non-persons (property)? Historical analysis: did the framers understand slavery as outside the equality clause''s domain or as a violation of it?',
    'If slavery is understood as categorically incompatible with even the narrowest equality principle, the restrictive originalist reading is internally contradictory—it cannot both assert propertied white male equality and slavery as constitutional rights without logical incoherence. If slavery is understood as compatible (because enslaved persons are not political actors/social contract parties), the reading preserves logical consistency but exposes the brutal narrowness of its beneficiary set. This omega surfaces the question of whether the constraint is defending a coherent principle or defending a distribution of power dressed in principle language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(status_of_slavery_within_equality_framework, conceptual, 'Logical compatibility between equality principle and slavery in the same constitutional framework.').

omega_variable(
    amendment_as_sole_expansion_mechanism,
    'Should constitutional expansion of equality rights require supermajority constitutional amendment, or should it also be available through judicial reinterpretation of the existing clause? Does the amendment requirement itself become an extractive mechanism once most citizens are excluded from the initial benefit class?',
    'Comparative analysis: track how amendment-only and judicial-reinterpretation regimes distribute power to expand rights; empirical observation of whether amendment processes are captured by interests favoring narrow interpretation; theoretical analysis of whether a supermajority requirement is democratically legitimate when it requires buy-in from beneficiaries of the narrow reading.',
    'If the amendment requirement becomes itself an extraction mechanism (beneficiaries of narrow reading can block expansion indefinitely), the suppression metric should increase—suppression is not just the active enforcement of exclusion, but the structural blocking of legitimate political mechanisms for revision. This omega surfaces the question of whether procedural fidelity (amendment requirement) can itself become a form of substantive extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_as_sole_expansion_mechanism, empirical, 'Whether amendment-only expansion creates procedural lock-in favoring narrow reading beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1787, equality_clause_scope__restrictive_originalist, theater_ratio, 1787, 0.15).
narrative_ontology:measurement(equa_tr_t1863, equality_clause_scope__restrictive_originalist, theater_ratio, 1863, 0.38).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__restrictive_originalist, theater_ratio, 1920, 0.41).
narrative_ontology:measurement(equa_tr_t1964, equality_clause_scope__restrictive_originalist, theater_ratio, 1964, 0.43).
narrative_ontology:measurement(equa_tr_t2000, equality_clause_scope__restrictive_originalist, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(equa_tr_t2025, equality_clause_scope__restrictive_originalist, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(equa_be_t1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 1787, 0.42).
narrative_ontology:measurement(equa_be_t1863, equality_clause_scope__restrictive_originalist, base_extractiveness, 1863, 0.61).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__restrictive_originalist, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(equa_be_t1964, equality_clause_scope__restrictive_originalist, base_extractiveness, 1964, 0.52).
narrative_ontology:measurement(equa_be_t2000, equality_clause_scope__restrictive_originalist, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(equa_be_t2025, equality_clause_scope__restrictive_originalist, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 1787, 0.55).
narrative_ontology:measurement(equa_su_t1863, equality_clause_scope__restrictive_originalist, suppression_requirement, 1863, 0.73).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__restrictive_originalist, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(equa_su_t1964, equality_clause_scope__restrictive_originalist, suppression_requirement, 1964, 0.64).
narrative_ontology:measurement(equa_su_t2000, equality_clause_scope__restrictive_originalist, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(equa_su_t2025, equality_clause_scope__restrictive_originalist, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__restrictive_originalist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, franchise_white_male_only).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, slavery_constitutional_legitimacy).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, amendment_process_supermajority_gate).

% DUAL FORMULATION NOTE:
% The equality clause scope is a contested kernel with three structurally distinct readings: restrictive_originalist (this story), expansive_universalist, and progressive_textualist. Each reading constitutes a separate constraint with distinct beneficiary sets, extraction profiles, and expansion thresholds. The readings are linked by network.affects_constraints and by omega variables documenting the kernel contest. Do not collapse the three readings into a single constraint—each has its own ε, its own victims/beneficiaries, and its own validity conditions. The disagreement is ABOUT WHICH READING IS CORRECT, not about observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__restrictive_originalist, institutional, 0.85).
constraint_indexing:directionality_override(equality_clause_scope__restrictive_originalist, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
