% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: State Power to Intervene in Religious Affairs for Social Reform (Principled Intervention Reading)
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the 'principled intervention' reading of the
 *   constitutional secularism kernel: the state may enter religious affairs
 *   where doing so advances social reform or protects weaker sections within
 *   a religious community, typically operationalized through an
 *   essential-practices test that distinguishes reformable social/secular
 *   incidents of religion from protected core religious doctrine. This is
 *   distinct from the strict_neutrality_reading (equal distance, no
 *   interference) and the reformist_reading (affirmative duty superseding
 *   religious autonomy entirely) — those are separate constraints with their
 *   own ε values, not alternative framings of this one. The principled
 *   intervention reading occupies a middle position: it legitimizes
 *   differential treatment case-by-case, tied to reform objectives, but
 *   retains (in principle) a boundary the reformist reading would dissolve.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.52).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.58).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "State Power to Intervene in Religious Affairs for Social Reform (Principled Intervention Reading)").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '478b3a10-86bf-418d-9ef1-74ccdb74bf6e').
narrative_ontology:cs_kernel_codification('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', formalized).
narrative_ontology:cs_authority_grounding('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', lineage).
narrative_ontology:cs_interpretation_layer_present('478b3a10-86bf-418d-9ef1-74ccdb74bf6e').
narrative_ontology:cs_reading_relation('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', foundational, reform_bounded_by_essentiality_test).
narrative_ontology:cs_axiom_status(reform_bounded_by_essentiality_test, holdable).
narrative_ontology:cs_axiom_grounding('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', reform_bounded_by_essentiality_test, conventional).
narrative_ontology:cs_axiom('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', secondary, religious_autonomy_yields_only_to_documented_weaker_section_harm).
narrative_ontology:cs_axiom_status(religious_autonomy_yields_only_to_documented_weaker_section_harm, holdable).
narrative_ontology:cs_axiom_grounding('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', religious_autonomy_yields_only_to_documented_weaker_section_harm, instrumental).
narrative_ontology:cs_reference_frame('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', constitutional_reform_mandate_founding_era).
narrative_ontology:cs_drift_state('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', contemporary_essentiality_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('478b3a10-86bf-418d-9ef1-74ccdb74bf6e', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, reform_minded_state_institutions).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, intra_community_reform_advocates).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, weaker_section_members_of_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_denominational_authorities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, orthodox_community_leadership).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, minority_religious_institutions_targeted_for_reform).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, essential_religious_practices_doctrine).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, social_reform_as_constitutional_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures and courts identify religious practices deemed oppressive to weaker sections (caste-based exclusion, gender-discriminatory temple entry, discriminatory personal law provisions) and enact statutes or issue rulings overriding religious governance on those points. They determine, through the essential-practices test, which religious claims survive scrutiny and which yield to reform objectives. They administer this power continuously and can expand or narrow its scope through subsequent doctrine.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, reform_minded_state_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Lower-caste worshippers denied temple access, women barred from certain shrines, or members subject to discriminatory community adjudication gain a state-backed avenue to challenge exclusion that community-internal politics would not have produced on its own. Their exit from the religious community is often costly or unavailable (social, economic, familial ties), so state intervention is frequently their only practical remedy.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, weaker_section_members_of_religious_communities, beneficiary,
    powerless, biographical, trapped, local).

% Religious governing bodies, trusts, and clergy lose authority to determine their own doctrine and practice wherever a court or legislature classifies the practice as non-essential or as oppressive to weaker sections. They can litigate the essentiality question but cannot exit the jurisdiction of the essential-practices doctrine; their institutional autonomy is contingent on state characterization of their own theology.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_denominational_authorities, payer,
    organized, generational, constrained, national).

% Local leaders whose social standing depends on maintaining traditional practice face state-mandated changes to admission, ritual, or governance rules. They experience the intervention as external imposition on internal community matters, with limited recourse beyond constitutional litigation that itself concedes the state's authority to adjudicate their theology.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, orthodox_community_leadership, payer,
    organized, generational, constrained, regional).

% Smaller or numerically minority religious groups face a structurally higher risk that their practices are singled out for reformist intervention, since majoritarian political consensus more readily forms around scrutinizing minority practice than majority practice. They lack the political weight to resist legislative attention and often lack the resources to sustain prolonged essentiality litigation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, minority_religious_institutions_targeted_for_reform, payer,
    moderate, biographical, trapped, national).

% Reform-minded members within the religious community itself who could not prevail through internal community processes gain external leverage through state intervention. Their voice inside the community was often marginal before intervention; they benefit from the doctrine but remain structurally outside the community's own decision-making apparatus even after the state acts on their behalf.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, intra_community_reform_advocates, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, intra_community_reform_advocates, excluded).

% Adjudicate essentiality claims and calibrate how far reform-oriented intervention may go without dissolving free exercise protections entirely. Their doctrine both authorizes and constrains the intervention power, and they bear responsibility for the coherence of the line between essential and non-essential practice over successive rulings.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, constitutional_courts, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an external check that can protect individuals structurally disadvantaged within their own religious community's internal governance — a coordination mechanism for cases where the community's own institutions have no incentive to reform practices harming their least powerful members.
% TRANSFER_FUNCTION: Moves interpretive and governance authority over contested religious practices from denominational and community leadership to state institutions (legislatures and courts), and moves practical relief (access, protection, legal remedy) from those leadership structures to weaker-section claimants.
% ABSENT_VOICES: The religious community's own internal reform mechanisms — where they exist but move slower than litigation — are effectively bypassed rather than strengthened; theological interpreters who might resolve the same disputes through doctrinal development are not consulted by courts applying an external essentiality test. Minority religious communities more broadly are underrepresented in the legislative and judicial bodies that decide which of their practices count as reformable.
% DISAPPEARANCE_RATIONALE: Without this intervention power, several currently state-mandated reforms to temple access, discriminatory personal law provisions, and community adjudication practices would revert to purely internal community politics, with much slower or absent redress for structurally disadvantaged members; religious institutions would regain full control over practices currently subject to essentiality review, and litigation strategies premised on state intervention would collapse.
% FOUNDING_PROBLEM: Post-independence constitutional drafters confronted entrenched caste-based exclusion, gender-discriminatory practice, and other intra-religious hierarchies that community self-governance had not reformed and showed no internal capacity to reform, alongside a competing commitment to religious freedom that could otherwise shield those hierarchies from any external remedy.
% FOUNDING_PROBLEM_CORROBORATION: Weaker-section claimants and reform litigants attest the founding problem remains live, citing persistent exclusionary practices reform intervention has addressed only partially. Religious authorities and several constitutional scholars outside the reform movement attest that the doctrine has drifted from remedying specific documented harms toward a general license for majoritarian political actors to reclassify disfavored minority practice as non-essential; comparative constitutional scholarship independent of both religious institutions and reform advocacy groups documents this drift as a recognized risk of essentiality-test jurisprudence.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52 at interval end) reflects genuine coordination value (protecting powerless community members who cannot secure reform internally) mixed with real transfer of governance authority away from religious institutions without their consent — a hybrid structure, not pure extraction. Suppression (0.58) captures the active enforcement machinery (essentiality litigation, legislative override, judicial doctrine) required to sustain state authority over contested religious practice against organized institutional resistance. Theater ratio is comparatively low (0.28) because the intervention mechanism does real adjudicative work, though the ratio rises over the measured interval as the essentiality test increasingly serves to launder majoritarian political preference under a reform label rather than to remedy documented harm to weaker sections.
 *
 * PERSPECTIVAL GAP:
 *   From the state/court seat, this reads as principled, bounded intervention correcting documented harms — a rope-like coordination function. From the organized religious authority seat, the same doctrine reads as an open-ended license for external reclassification of internal practice, sustained only by continuous litigation and enforcement — closer to tangled_rope or, where the essentiality test drifts furthest from documented harm, snare. The engine should register this divergence structurally: the beneficiary/victim declarations and differing exit_options for state vs. religious-authority seats are the mechanism, not an adjudication of which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker-section members and intra-community reform advocates are structural beneficiaries — they gain a remedy unavailable through internal community process, and their limited exit options (trapped/constrained) make the external avenue especially valuable. Religious denominational authorities, orthodox leadership, and targeted minority institutions are structural targets — the same doctrine that provides redress to the powerless removes governance authority from the organized community structure without its consent, and their exit is constrained (litigation concedes state authority) or trapped (minority institutions lack political weight to escape scrutiny). The state institutions and courts sit as agenda-setters with analytical exit, administering rather than bearing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (entrenched, self-perpetuating intra-community hierarchies with no internal reform capacity) remains partially live for some practices and substantially resolved for others, which is why founding_problem_status is authored as 'contested' rather than 'live' or 'dead.' Classifying this as tangled_rope rather than snare or rope reflects that both the coordination function (remedy for the powerless) and the asymmetric extraction (majoritarian reclassification of minority practice) are simultaneously real and structurally coupled through the same essentiality-test mechanism — collapsing it to either pure category would erase one of the two documented dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    essentiality_test_capture_vs_remedy,
    'Does the essential-practices doctrine, as actually applied, track documented harm to weaker sections, or has it become a vehicle for majoritarian political actors to reclassify disfavored minority religious practice as non-essential regardless of actual harm?',
    'Longitudinal case-coding of essentiality rulings against (a) documented harm evidence presented and (b) the relative political power of the community whose practice was reclassified; a pattern of reclassification correlating with minority/politically weak status rather than harm evidence would support the capture reading.',
    'If capture-dominant, this reading''s effective classification shifts toward snare for minority religious institutions specifically, even while remaining tangled_rope in aggregate; if remedy-dominant, the tangled_rope classification is well-supported as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essentiality_test_capture_vs_remedy, empirical, 'Whether essentiality doctrine tracks harm remedy or majoritarian political capture.').

omega_variable(
    kernel_reading_boundary_stability,
    'Is the boundary between this principled_intervention_reading and the reformist_reading (which would supersede religious autonomy entirely) stable in practice, or does sustained application of the essentiality test tend to erode toward the reformist position over time?',
    'Track whether successive essentiality rulings progressively narrow what counts as ''essential'' religious practice, which would indicate doctrinal drift from the bounded principled_intervention_reading toward the unconditional reformist_reading.',
    'If the boundary erodes, this reading is not a stable independent position but a way-station toward the reformist_reading, and the ''reform correction'' framing of the boundary itself may be doing less work than the doctrine claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_stability, conceptual, 'Whether the principled intervention reading is a stable equilibrium or drifts toward the reformist reading over time.').

omega_variable(
    internal_reform_capacity_counterfactual,
    'Would internal community reform movements have eventually produced comparable remedies for weaker sections absent external state intervention, on a longer but community-legitimated timeline?',
    'Comparative study of religious communities/denominations that reformed comparable practices without state intervention, examining what internal conditions enabled reform versus what conditions in the intervened communities blocked it.',
    'If internal reform was plausible on a longer horizon, some of the state intervention''s claimed coordination value is better characterized as accelerating a process already underway rather than solving a problem the community could not solve itself — reducing (but not eliminating) the genuine coordination-function claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_reform_capacity_counterfactual, conceptual, 'Whether internal community reform was a viable counterfactual to state intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t14, constitutional_secularism__principled_intervention_reading, theater_ratio, 14, 0.16).
narrative_ontology:measurement(cons_tr_t28, constitutional_secularism__principled_intervention_reading, theater_ratio, 28, 0.19).
narrative_ontology:measurement(cons_tr_t42, constitutional_secularism__principled_intervention_reading, theater_ratio, 42, 0.22).
narrative_ontology:measurement(cons_tr_t56, constitutional_secularism__principled_intervention_reading, theater_ratio, 56, 0.25).
narrative_ontology:measurement(cons_tr_t70, constitutional_secularism__principled_intervention_reading, theater_ratio, 70, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t14, constitutional_secularism__principled_intervention_reading, base_extractiveness, 14, 0.36).
narrative_ontology:measurement(cons_be_t28, constitutional_secularism__principled_intervention_reading, base_extractiveness, 28, 0.42).
narrative_ontology:measurement(cons_be_t42, constitutional_secularism__principled_intervention_reading, base_extractiveness, 42, 0.46).
narrative_ontology:measurement(cons_be_t56, constitutional_secularism__principled_intervention_reading, base_extractiveness, 56, 0.49).
narrative_ontology:measurement(cons_be_t70, constitutional_secularism__principled_intervention_reading, base_extractiveness, 70, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cons_su_t14, constitutional_secularism__principled_intervention_reading, suppression_requirement, 14, 0.44).
narrative_ontology:measurement(cons_su_t28, constitutional_secularism__principled_intervention_reading, suppression_requirement, 28, 0.49).
narrative_ontology:measurement(cons_su_t42, constitutional_secularism__principled_intervention_reading, suppression_requirement, 42, 0.53).
narrative_ontology:measurement(cons_su_t56, constitutional_secularism__principled_intervention_reading, suppression_requirement, 56, 0.56).
narrative_ontology:measurement(cons_su_t70, constitutional_secularism__principled_intervention_reading, suppression_requirement, 70, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'constitutional secularism / state-religion intervention' kernel per the ε-invariance principle. strict_neutrality_reading authors near-zero differential-treatment extraction (equal-distance doctrine, minimal reform-driven state authority expansion). reformist_reading authors substantially higher extraction against religious institutional autonomy generally (affirmative duty superseding autonomy claims, no essentiality boundary). This principled_intervention_reading sits between them: bounded differential treatment tied to documented reform objectives, retaining (contestably) a doctrinal limit the reformist reading dissolves. All three share the underlying kernel_id constitutional_secularism but are authored as separate constraints with independent ε, stakeholder sets, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
