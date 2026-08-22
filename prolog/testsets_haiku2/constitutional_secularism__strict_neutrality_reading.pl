% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secular Neutrality: Equal Distance from All Religions
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the strict-neutrality reading of the
 *   constitutional secularism kernel. The state maintains formal equal
 *   distance from all religions: no preferential treatment, no state
 *   interference in religious affairs (except where secular interests are
 *   directly implicated — e.g., protection of children from abuse). The
 *   reading protects religious minorities from majoritarian imposition and
 *   prevents theocratic movements from capturing state power. It
 *   simultaneously denies the state capacity to intervene in oppressive
 *   religious practices within communities, creating a protection gap for
 *   marginalized individuals whose harms are deemed 'internal matters'. The
 *   constraint is claimed as tangled_rope because it coordinates plural
 *   coexistence (genuine benefit to minorities) while extracting the state's
 *   practical reform capacity from domains defined as 'religious'. The
 *   claim/metric gap is intentional: extractiveness is high (0.62) because
 *   the constraint substantially constrains legislative and executive power,
 *   and suppression is moderate (0.41) because theocratic movements actively
 *   resist the doctrine — the neutrality principle must be defended, not
 *   merely accepted.
 *
 * KEY AGENTS:
 *   - Constitutional court: institutional agenda-setter; defines what neutrality means operationally through doctrine
 *   - Religious minorities: powerless beneficiaries; protected from majority imposition by the equal-distance principle
 *   - Theocratic movements: organized payers; cannot use state apparatus for religious advancement; politically constrained
 *   - Secular constituencies: organized beneficiaries; maintain the institutional dominance that interprets neutrality; can exit if erosion threatens
 *   - Communally marginalized groups: powerless and identity-locked payers; excluded from reform by the neutrality doctrine; experience internal oppression the state will not remedy
 *   - Legislative supermajority: organized payers; cannot legislate violations of secular neutrality even with electoral mandate; constrained by constitutional review
 *   - Competing secular readings: institutionally excluded; their arguments for state intervention are ruled out-of-order by the neutrality doctrine itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.62).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.41).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secular Neutrality: Equal Distance from All Religions").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional/political").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '66f87c90-8c3a-4776-86ab-ca50b0d5a62f').
narrative_ontology:cs_kernel_codification('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', formalized).
narrative_ontology:cs_authority_grounding('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', lineage).
narrative_ontology:cs_interpretation_layer_present('66f87c90-8c3a-4776-86ab-ca50b0d5a62f').
narrative_ontology:cs_reading_relation('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', foundational, state_non_interference_in_religious_matters).
narrative_ontology:cs_axiom_status(state_non_interference_in_religious_matters, holdable).
narrative_ontology:cs_axiom_grounding('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', state_non_interference_in_religious_matters, conventional).
narrative_ontology:cs_axiom('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', foundational, equal_legal_distance_enables_minority_protection).
narrative_ontology:cs_axiom_status(equal_legal_distance_enables_minority_protection, holdable).
narrative_ontology:cs_axiom_grounding('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', equal_legal_distance_enables_minority_protection, empirically_contingent).
narrative_ontology:cs_reference_frame('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', established_religious_neutrality_doctrine).
narrative_ontology:cs_drift_state('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', contemporary_reformist_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('66f87c90-8c3a-4776-86ab-ca50b0d5a62f', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, secular_constituencies).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, theocratic_movements).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, communally_marginalized_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, communally_marginalized_groups).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, legislative_supermajority).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, secular_state_doctrine).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, religious_freedom_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the secular neutrality principle, deciding which state actions violate equidistance (e.g., bans on religious dress in certain contexts, subsidy decisions for religious institutions, family law jurisdiction). The court sets the operative doctrine through rulings and must balance competing interpretations of neutrality itself — whether neutrality means absolute non-recognition, equal recognition, or context-sensitive accommodation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).

% Gain formal legal protection from majority religious domination through the equal-distance principle: the state cannot privilege the majority faith in law, education, or governance. Their autonomy is secured as long as the court interprets neutrality expansively (protecting their worship, dress, family practices from state interference). They risk being outvoted on reforms if the majority rediscovers religious justifications for policies.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities, beneficiary,
    powerless, generational, constrained, national).

% Cannot use state power to advance religious agendas that would privilege their faith or impose its norms. They bear the cost of secularist doctrine: cannot compel religious education in state schools, cannot establish religious courts with binding jurisdiction, cannot use public funds preferentially for their institutions. Their recourse is political organizing to shift the electorate or the court majority.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, theocratic_movements, payer,
    organized, generational, constrained, national).

% Benefit from the state's formal non-religious character: civil law uncoupled from religious doctrine, education based on reason and evidence, public offices open on grounds of qualification not faith. Can exit via international migration if secularism erodes; have political organizing capacity to defend the principle. Their benefit is primarily negative (absence of theocratic constraint) rather than extraction of resources.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, secular_constituencies, beneficiary,
    organized, generational, mobile, national).

% Within patriarchal or hierarchical faith communities, the strict neutrality principle denies them state protection from internal oppression (forced marriage, inheritance inequality, bodily autonomy violations) justified by religious tradition. The state does not intervene in 'religious matters' even when the harm is severe. Yet they also benefit from the protection of religious minorities, which prevents the majority faith from imposing its own hierarchies on the whole population. Their exit options are severely constrained by family ties, community identity, economic dependence, and (often) lack of alternative shelter or livelihood.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, communally_marginalized_groups, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, communally_marginalized_groups, beneficiary).

% Cannot enact laws that the court deems to violate secular neutrality, even with strong electoral mandate. Their legislative power is constrained by constitutional review — policies framed as advancing 'the general welfare' may be struck down if judges deem them to embody religious preference. Political will to govern is limited by the interpretive frame.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, legislative_supermajority, payer,
    organized, generational, constrained, national).

% Principled intervention and reformist readings argue the state should actively intervene in religious affairs to eliminate oppressive practices. This strict neutrality reading systematically excludes their framing from valid discourse: saying 'the state has a duty to reform patriarchal religious practice' is ruled out as violating neutrality. Their exclusion is doctrinal and constitutional.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, competing_reform_readings, excluded,
    institutional, generational, constrained, national).

% Views the constraint from outside, attending to its structural logic: which groups benefit from equal distance, which bear costs, what paradoxes emerge (protection of minority autonomy that shelters internal oppression; impossibility of true neutrality when unequal starting positions exist).
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__strict_neutrality_reading, secular_constituencies).
narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of multi-faith coexistence in a shared state: instead of competing for state favor and resource allocation, all religions are placed on equal legal footing. Removes incentives for any group to seek state power to impose its doctrine; protects plural belief systems from majority imposition.
% TRANSFER_FUNCTION: Transfers political power away from theocratic movements and religious majorities (who lose the ability to use state apparatus for religious purposes) and toward minorities (who gain formal legal parity). Also transfers to secular constituencies the stability of predictable, faith-neutral governance. Transfers FROM the state's practical capacity for social reform in domains framed as 'religious'.
% ABSENT_VOICES: Communally marginalized groups (women, low-caste members, LGBTQ+ individuals within conservative faiths) are structurally excluded from the reform-vs-autonomy tradeoff debate. Their harms within religious communities are treated as 'internal matters' — the constraint prevents their voice from triggering state intervention. They would argue for the state's affirmative duty to protect them, but that argument is ruled out by the neutrality doctrine.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished overnight, the state could immediately intervene in religious affairs (education, family law, ritual practice). Majorities could seek to legislate their faith into law; minorities would lose the constitutional shield preventing such imposition. Religious minorities would either organize politically for protection or migrate. Internal marginalization within faith communities might become subject to state scrutiny, or might face renewed enforcement by majority power. The entire configuration of power and autonomy would reorganize.
% FOUNDING_PROBLEM: Religious establishment and sectarian violence: states using power to privilege one faith and suppress others. The founding problem is prevention of theocratic control and religious warfare through formal non-interference.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on sectarian conflict and institutional secularism (outside the benefiting parties) confirms the founding problem was live. However, the question whether the founding problem persists is deeply contested: theocratic movements argue secularism itself is now the threat; reformist movements argue the problem has morphed (not suppressed minorities but oppressed internal groups now require intervention). No single external authority commands consensus.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is high because the constraint substantially constrains the state's practical decision-making space: legislators cannot frame policies in religious terms or provide preferential support for any faith, even if majorities favor it. The constraint persists not because it is inevitable but because the court enforces it, so suppression (0.41) is moderate — theocratic movements actively resist, testing the doctrine's boundaries (prayer in schools, religious dress, family law jurisdiction, minority-protection remedies). Theater is low-to-moderate (0.28): the neutrality principle is genuinely implemented in many contexts (civil law uncoupled from doctrine, public office allocation, state education), but a growing share of court energy defends the doctrine against encroachment rather than discovering new applications. The measurement series traces a rising extraction trajectory from t0 to t=32, where extraction plateaus as the doctrine stabilizes in institutional memory. Suppression requirement is stable because maintaining neutrality in a contested environment requires constant defense — the requirement does not decay as alternatives are foreclosed (unlike a snare, where suppression can diminish once resistance is broken) because theocratic movements retain political organizing capacity and continue to challenge the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The court (agenda-setter) experiences this as genuine coordination: the neutrality doctrine it maintains solves the multi-faith coexistence problem and enables plural flourishing. Theocratic movements experience it as extraction: they cannot advance their vision for the polity and must watch the state disestablish religious influences they value. Religious minorities experience it as protection: they benefit from being placed on legal parity. But communally marginalized groups (women, LGBTQ+ individuals, low-caste members within conservative faiths) experience it as abandonment: the state claims not to interfere in 'religious matters' and thus denies them protection from internal oppression. From the analytical seat, the constraint is tangled_rope precisely because it solves a real coordination problem (multi-faith coexistence) while creating an asymmetric cost structure (protection of community autonomy that shelters internal domination). Secular constituencies see the doctrine as foundational; theocratic movements see it as a secular takeover; marginalized individuals within faith communities see it as a betrayal.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious minorities (powerless, constrained exit) are clear beneficiaries: d approaches 0 (they gain from the equal-distance principle; the constraint subsidizes their autonomy relative to majoritarian power). Theocratic movements (organized, constrained exit) are clear targets: d approaches 1 (they cannot use state power; their exit is constrained by the institutional dominance of the secular reading). Secular constituencies (organized, mobile exit) are partial beneficiaries: d approaches 0.25-0.35 (they benefit from the doctrine and can exit if it erodes, giving them leverage). Communally marginalized groups (powerless, identity-locked exit) are unusual targets: d approaches 0.7-0.8. They are formally protected by the minority-protection function of the constraint (it prevents majority imposition on their faiths), but they pay the cost of internal oppression the state will not remedy. Their directionality is high-target because their practical options are narrower and their experience is systematically harmed by the exclusion of reform. Legislative supermajority is constrained-target (d around 0.65): they cannot govern according to the preferences they represent if those preferences are deemed to embody religious preference.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict-neutrality reading authorizes itself through the founding problem: prevention of sectarian conflict and theocratic control. The founding problem's status is contested because theocratic movements now argue the problem has reversed (secularism is the threat) and reformist movements argue the problem has transformed (not inter-faith conflict but intra-faith oppression now requires remedy). The disappearance verdict is world_rearranges: if neutrality vanished, state power would immediately be available for religious purposes and the entire coalition that benefits from equal-distance protection would reorganize. However, the theater_ratio trajectory (rising from 0.18 to 0.28) suggests an increasing ratio of doctrine-defense to novel application — the constraint may be accumulating performative energy (repeated citation of the neutrality principle, court rhetoric emphasizing its importance) while its actual reform capacity diminishes. If theater continues rising above 0.40-0.50, the constraint drifts toward piton territory: a real historical function (preventing theocratic takeover) that persists in institutional memory and theatrical defense but whose living coordination function has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_reading_divergence,
    'Is strict neutrality the correct reading of the secular state kernel, or do principled intervention and reformist readings offer structurally superior instantiations of the same constitutional commitment?',
    'Comparative analysis of each reading''s handling of internal marginalization within faith communities, minority protection from majority norms, and state capacity for reform. The test is whether one reading''s axioms foreclose the others'' core premises or whether all three remain internally coherent rival readings of the same ambiguous kernel.',
    'If strict neutrality is the sole coherent reading, the sibling readings are false doctrines and should be rejected. If the readings are genuinely coexistent (each coherent under different normative assumptions about state duty), the constraint properly sits in tangled_rope territory with substantial contestation about its legitimacy. If the reformist reading is held by an ascendant coalition, the constraint drifts toward snare (extraction via prevented intervention, benefiting patriarchal authority).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_reading_divergence, conceptual, 'Whether this reading and its siblings are genuinely coexistent or whether one logically forecloses the others.').

omega_variable(
    internal_marginalization_protection_gap,
    'Does the strict neutrality principle create a protection gap for individuals oppressed within their own faith communities (forced marriage, bodily autonomy violations, inheritance discrimination)? Is that gap structurally necessary to the principle or a remediable design flaw?',
    'Empirical: jurisdictions adopting the strict neutrality reading show systematically higher rates of internal-community harm to marginalized groups compared to jurisdictions adopting the reformist reading (controlling for overall marginalization levels). Normative: does protecting religious autonomy from state interference logically require non-intervention in internal harms, or can the state protect individuals without imposing religious doctrine?',
    'If the gap is empirically large and remediable (reformist interventions reduce harm without triggering the foreclosure dynamics the neutrality reading predicts), the constraint''s classification shifts toward snare or piton (protection of internal oppression disguised as religious liberty). If the gap is unavoidable within any secular framework, the tradeoff is genuine and the tangled_rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_marginalization_protection_gap, empirical, 'Whether strict neutrality necessarily protects internal community oppression or whether that protection is a remediable side effect.').

omega_variable(
    theocratic_pressure_trajectory,
    'Will sustained theocratic movement organizing eventually shift the court majority, narrowing the neutrality doctrine toward majoritarian religious accommodation, or will secular constituencies maintain institutional control of the interpretation?',
    'Tracking court composition, electoral outcomes, legislative attempts to narrow or broaden neutrality, and shifts in what the court deems ''religious'' vs. ''secular'' policy.',
    'If theocratic pressure succeeds and the doctrine narrows, the constraint drifts from tangled_rope (contested but stabilized) toward snare (majoritarian extraction justified by narrowed ''neutrality''). If secular constituencies entrench control, the constraint may ossify into piton (performance of neutrality without functional reform capacity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theocratic_pressure_trajectory, empirical, 'Whether the institutional control of the neutrality reading persists against theocratic pressure.').

omega_variable(
    true_neutrality_impossibility,
    'Is true neutrality (equal distance from all religions) even conceptually possible, or does the very definition of neutrality embody secular epistemology and marginalizes faith-based reasoning as inherently non-neutral?',
    'Philosophical analysis of what ''equal distance'' means when religions make incompatible metaphysical and ethical claims. Can the state treat incompatible worldviews as genuinely equal without treating one as ''true'' and others as ''false''? Or does neutrality necessarily privilege the secular stance (treating all religions as equivalent human constructs rather than differential access to truth)?',
    'If neutrality is impossible in principle, the constraint''s claim is necessarily false — it would be a snare disguised as rope (privileging secular reasoning while denying it does so). If neutrality is achievable through procedural equipoise (same treatment regardless of worldview compatibility), the constraint is coherent. This is primarily a conceptual omega but has practical consequences for whether marginalized internal groups within faiths (who may not accept secular framing) are structurally excluded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_neutrality_impossibility, conceptual, 'Whether neutrality is logically possible or inherently privileges secular reasoning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(cons_tr_t0, projected).
narrative_ontology:measurement(cons_tr_t8, constitutional_secularism__strict_neutrality_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(cons_tr_t8, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_secularism__strict_neutrality_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_secularism__strict_neutrality_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t32, constitutional_secularism__strict_neutrality_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement_basis(cons_tr_t32, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__strict_neutrality_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(cons_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(cons_be_t0, projected).
narrative_ontology:measurement(cons_be_t8, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement_basis(cons_be_t8, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t32, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(cons_be_t32, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(cons_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cons_su_t0, projected).
narrative_ontology:measurement(cons_su_t8, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement_basis(cons_su_t8, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t32, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement_basis(cons_su_t32, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(cons_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__strict_neutrality_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the constitutional-secularism kernel. The kernel asks whether the state should maintain equal distance from all religions. The strict-neutrality reading (this file) says yes: equal distance, no interference. The principled-intervention reading says the state may intervene to advance reform while preserving minority autonomy. The reformist reading says the state must eliminate oppressive practices, superseding autonomy. Each reading is a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification. All three are linked via network.affects_constraints to model the kernel's contested nature and the downstream pressure each reading exerts on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__strict_neutrality_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
