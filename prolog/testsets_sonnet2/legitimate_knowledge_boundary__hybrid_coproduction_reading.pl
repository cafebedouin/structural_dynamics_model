% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Co-Production Knowledge Legitimacy Standard (Dual Validation Reading)
 *   domain: epistemology/science_technology_studies/political_theory
 *
 * SUMMARY:
 *   This story authors the hybrid co-production reading of the contested
 *   legitimate-knowledge-boundary kernel: the claim that legitimate knowledge
 *   requires BOTH methodological rigor AND experiential validity, integrated
 *   through formal co-production processes. Unlike the credentialed-expertise
 *   reading (methodological review alone) or the experiential-pluralism
 *   reading (community validation as sufficient), this reading erects a
 *   dual-gate standard — knowledge must pass through both forms of
 *   validation, mediated by co-production infrastructure, to count as
 *   legitimate. Over the twenty-four year interval studied, the standard has
 *   moved from addressing a genuine integration problem toward increasingly
 *   functioning as a credential controlled by an emerging class of boundary
 *   organizations, program directors, and dedicated funding streams — a
 *   coordination function riding alongside a growing extraction function that
 *   channels resources toward those already positioned inside co-production
 *   infrastructure and away from unaffiliated knowledge holders on one side
 *   and purist researchers on the other.
 *
 * KEY AGENTS:
 *   - coproduction_program_directors: institutional agenda-setters who define and administer the dual-validation gate
 *   - boundary_organization_intermediaries: organized beneficiaries whose professional existence depends on the standard persisting
 *   - community_coproduction_partners: moderate-power payers/beneficiaries who do the uncompensated translation labor the standard requires
 *   - unaffiliated_community_knowledge_holders: powerless payers permanently outside the legitimacy boundary absent institutional selection
 *   - credentialed_expertise_purists and experiential_pluralism_advocates: excluded seats representing the two rejected sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.52).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.46).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Co-Production Knowledge Legitimacy Standard (Dual Validation Reading)").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'eeafcfaf-3b29-471d-979a-2575056bf439').
narrative_ontology:cs_kernel_codification('eeafcfaf-3b29-471d-979a-2575056bf439', distributed).
narrative_ontology:cs_authority_grounding('eeafcfaf-3b29-471d-979a-2575056bf439', practice).
narrative_ontology:cs_interpretation_layer_present('eeafcfaf-3b29-471d-979a-2575056bf439').
narrative_ontology:cs_reading_relation('eeafcfaf-3b29-471d-979a-2575056bf439', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('eeafcfaf-3b29-471d-979a-2575056bf439', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_axiom('eeafcfaf-3b29-471d-979a-2575056bf439', foundational, dual_validation_necessity).
narrative_ontology:cs_axiom_status(dual_validation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('eeafcfaf-3b29-471d-979a-2575056bf439', dual_validation_necessity, conventional).
narrative_ontology:cs_axiom('eeafcfaf-3b29-471d-979a-2575056bf439', secondary, coproduction_process_as_epistemic_precondition).
narrative_ontology:cs_axiom_status(coproduction_process_as_epistemic_precondition, holdable).
narrative_ontology:cs_axiom_grounding('eeafcfaf-3b29-471d-979a-2575056bf439', coproduction_process_as_epistemic_precondition, instrumental).
narrative_ontology:cs_reference_frame('eeafcfaf-3b29-471d-979a-2575056bf439', post_positivist_integration_consensus).
narrative_ontology:cs_drift_state('eeafcfaf-3b29-471d-979a-2575056bf439', contemporary_participatory_research_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eeafcfaf-3b29-471d-979a-2575056bf439', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_program_directors).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, boundary_organization_intermediaries).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, funders_of_participatory_research).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, unaffiliated_community_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, junior_researchers_without_coproduction_access).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, communities_without_institutional_partners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers_in_coproduction_teams).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_coproduction_partners).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers_in_coproduction_teams).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_coproduction_partners).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, knowledge_integration_thesis).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_epistemic_superiority_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the co-production protocols that define what counts as legitimate hybrid knowledge — who must be at the table, what counts as 'genuine' community involvement, how methodological review and experiential validation are weighted against each other. They control the certification pipeline (grants, publication venues, policy uptake) that legitimate knowledge must pass through, and their institutional standing is built on being the arbiters of the integration process itself.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_program_directors, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_program_directors, beneficiary).

% Staff the intermediary organizations (community-academic liaison offices, participatory action research centers) that translate between methodological and experiential registers. Their jobs exist because the dual-validation requirement exists; they have professional and financial stakes in co-production remaining the required path rather than becoming optional.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, boundary_organization_intermediaries, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, boundary_organization_intermediaries, agenda_setter).

% Foundations and agencies that have built funding streams, review criteria, and reputational capital around requiring co-production as a condition of grant legitimacy. They benefit from the standard's persistence because it justifies their programmatic infrastructure and distinguishes their portfolios from purely academic or purely community-based funders.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, funders_of_participatory_research, beneficiary,
    institutional, generational, arbitrage, national).

% Trained academics who must now spend years building trust relationships and co-design processes with communities before methodologically rigorous work can even begin, on top of meeting disciplinary rigor standards. They gain legitimacy and funding access from participating but bear substantial time and career-timeline costs relative to purely disciplinary peers.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers_in_coproduction_teams, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers_in_coproduction_teams, beneficiary).

% Community members and organizations recruited into formal co-production roles. They gain a genuine voice in framing questions and validating findings against lived experience, but bear the burden of translating their knowledge into forms institutions recognize, participate largely on institutional timelines and terms, and are often uncompensated or under-compensated for labor that credentialed partners are paid for.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_coproduction_partners, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_coproduction_partners, beneficiary).

% Hold experiential knowledge but lack access to the formal co-production infrastructure (grants, institutional partnerships, boundary organizations) required to have that knowledge certified as 'integrated' legitimate knowledge. Their knowledge claims are treated as illegitimate or as raw material awaiting validation rather than as knowledge in its own right, unless and until an institutional partner selects them for a co-production process they do not control.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, unaffiliated_community_knowledge_holders, payer,
    powerless, biographical, trapped, local).

% Early-career scholars, often without the institutional standing or existing community relationships to stand up a co-production process, are structurally disadvantaged relative to established researchers who already run boundary organizations or have longstanding partnerships. The dual-validation requirement raises the entry cost to producing 'legitimate' knowledge in fields where the standard has become dominant.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, junior_researchers_without_coproduction_access, payer,
    moderate, biographical, constrained, national).

% Entire communities that lack a nearby university, funded intermediary, or existing research relationship find their locally-generated knowledge permanently excluded from the legitimacy pathway, not because it lacks rigor or experiential grounding but because no co-production infrastructure has been built to certify it. Exit means either remaining outside the legitimacy boundary or waiting to be selected by an institution.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, communities_without_institutional_partners, payer,
    powerless, generational, trapped, regional).

% Researchers and institutions committed to methodological rigor as the sole legitimacy criterion object that co-production dilutes rigor with unverifiable experiential claims and slows inquiry with process requirements. They are present in disciplinary debates but structurally sidelined from co-production funding streams and journals that have adopted the hybrid standard as their gatekeeping criterion.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_purists, excluded,
    powerful, biographical, constrained, national).

% Advocates for community and experiential knowledge as self-sufficiently legitimate object that the co-production standard still subordinates experiential knowledge to eventual methodological ratification — that 'integration' in practice means experiential claims must survive methodological translation to count, reproducing the very hierarchy pluralism sought to dismantle. They participate in co-production processes under protest rather than by conviction.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_pluralism_advocates, excluded,
    organized, biographical, constrained, regional).

% Assess whether a given knowledge claim satisfies the co-production standard for legitimacy in grant review, publication, and policy uptake. Their adjudication decisions directly determine which knowledge claims pass the boundary and which are treated as illegitimate or preliminary.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, policy_evaluators_and_journal_reviewers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a genuine problem where methodologically rigorous research produces findings communities reject as inapplicable or extractive, and community-validated experiential knowledge is dismissed by policy and scientific institutions as anecdotal — by requiring a joint process where neither form of validation alone suffices, in principle producing more actionable, trusted, and empirically grounded knowledge than either alone.
% TRANSFER_FUNCTION: Moves legitimacy-granting authority and the resources attached to it (grants, publication access, policy standing) toward whichever researchers and communities can afford the infrastructure of formal co-production, and away from unaffiliated knowledge holders, poorly-resourced communities, and researchers without existing community relationships — even when their knowledge independently satisfies rigor or experiential validity on its own terms.
% ABSENT_VOICES: Unaffiliated community knowledge holders and communities without institutional partners are structurally outside every co-production table by definition — they cannot advocate for recognition of their knowledge because the standard requires the very institutional relationship they lack. Credentialed purists and pluralism advocates are present in disciplinary debate but sidelined from the funding and publication infrastructure that has adopted the hybrid standard as gatekeeping criterion.
% DISAPPEARANCE_RATIONALE: Program directors, intermediaries, and funders would say the world rearranges badly: co-production infrastructure, trust relationships, and integrated knowledge products built over years would collapse back into disconnected methodological and experiential silos. Unaffiliated knowledge holders and under-resourced communities would say little changes for them either way, since the standard's benefits accrue to institutionally-connected parties regardless; some would say its disappearance removes an artificial gate currently blocking recognition of knowledge that is already both rigorous and experientially grounded.
% FOUNDING_PROBLEM: Methodologically rigorous research repeatedly produced policy failures and community harm when it ignored local, experiential, and indigenous knowledge; conversely, experiential and community claims were repeatedly dismissed by policymakers and scientific gatekeepers as unverifiable, leaving both real problems (environmental justice, public health disparities, resource management) poorly served by either epistemic mode alone.
% FOUNDING_PROBLEM_CORROBORATION: Independent program evaluations (e.g., of participatory environmental health and indigenous co-management research) attest that the integration problem was real and that early co-production efforts produced measurably better-adopted policy outcomes in specific cases — this corroboration comes from evaluators outside the program-director and funder seats. However, those same evaluations, and separate critiques from science-and-technology-studies scholars not employed by co-production programs, report that the standard has since become a gatekeeping credential in its own right, used to exclude both rigorous non-participatory research and legitimate community knowledge that lacks institutional sponsorship — suggesting the founding problem has been partially supplanted by an administrative function serving the intermediary apparatus itself.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52 at interval end) reflects that roughly half the standard's operation now channels resources and legitimacy toward institutionally-positioned actors rather than toward the integration function itself — moderate, not severe, because genuine coordination gains (better-adopted policy, reduced community harm from purely extractive research) remain real and documented. Suppression (0.46) is moderate: the standard does not forcibly bar alternative knowledge claims from existing, but it does structurally exclude unaffiliated and under-resourced parties from the legitimacy pathway by design, and its accessibility_collapse (0.50) reflects that once co-production is understood, alternative direct paths to legitimacy (pure rigor, pure experiential standing) are substantially narrowed within the fields that have adopted the standard, though not eliminated globally. Theater ratio rises from 0.12 to 0.38 over the interval — a meaningful but not yet dominant share of activity is now performative compliance (checkbox community consultation, symbolic co-authorship) rather than genuine integration, tracking the standard's drift from problem-solving mechanism toward credentialing apparatus. Resistance (0.58) is comparatively high because both excluded sibling-reading communities actively contest the hybrid standard's legitimacy from opposite directions.
 *
 * PERSPECTIVAL GAP:
 *   Program directors and funders experience this as a rope: a coordination mechanism they built to solve a real epistemic and political problem, now functioning well and deserving of continued investment. Unaffiliated knowledge holders and under-resourced communities experience the identical structure as something closer to a snare: a gate they cannot pass regardless of the actual rigor or experiential grounding of their knowledge, because they lack the institutional relationship the gate requires. Credentialed researchers inside co-production teams experience genuine hybrid costs and benefits — real career advantage from participating, real burden from the dual-validation requirement relative to purely disciplinary peers. This divergence is exactly the tangled-rope signature: a real coordination function (integration solves genuine problems) running through the same structure as asymmetric extraction (resources concentrate on institutionally-positioned actors).
 *
 * DIRECTIONALITY LOGIC:
 *   Program directors, boundary intermediaries, and funders are declared beneficiaries because the standard's persistence is structurally tied to their institutional and professional standing — this reading treats their d as low, near the beneficiary end. Unaffiliated knowledge holders, junior researchers without infrastructure access, and communities lacking institutional partners are declared victims: their knowledge is treated as illegitimate or provisional purely due to lack of access to co-production infrastructure, independent of its actual epistemic merit, so their d sits near the full-target end. Credentialed researchers in co-production teams and community co-production partners are dual-positioned (payer + beneficiary secondary roles) because they bear real costs (time, translation labor, uncompensated participation) while also gaining real legitimacy and resource access — their directionality is closer to symmetric than either pure beneficiary or pure victim seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare or rope alone) prevents two mislabeling errors symmetrically: it prevents dismissing the standard as pure extraction (which would erase the genuine coordination gains documented in early participatory environmental-health and indigenous co-management cases), and it prevents certifying the standard as pure coordination (which would launder the demonstrated exclusion of unaffiliated knowledge holders and the rising theater ratio as mere growing pains). The founding_problem_status of 'contested' with corroboration from evaluators outside the program-director/funder seats captures precisely this: the founding problem was real, is partially still live, and the arrangement has also partially drifted toward serving the administrative apparatus built to solve it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coproduction_kernel_reading_choice,
    'Is the hybrid co-production reading the correct account of what makes knowledge legitimate, or is it a compromise formation that inherits the exclusions of both single-criterion readings while adding new infrastructure-access exclusions of its own?',
    'This is the committer-axis ambiguity for the legitimate_knowledge_boundary kernel: the credentialed_expertise_reading would hold that methodological rigor alone, validated by peer review, is sufficient and that requiring experiential validation dilutes rigor without epistemic gain. The experiential_pluralism_reading would hold that community validation is sufficient on its own terms and that requiring methodological ratification subordinates experiential knowledge to a hierarchy pluralism seeks to dismantle. No empirical test adjudicates between these three readings because they encode different normative theories of what legitimacy IS, not different empirical claims about a shared criterion.',
    'Adopting the credentialed_expertise_reading would eliminate this constraint''s beneficiary/victim structure entirely (unaffiliated knowledge holders would not be victims of a boundary that doesn''t require institutional co-production access) while creating a different one (community knowledge holders excluded by lacking credentials). Adopting the experiential_pluralism_reading would eliminate the co-production infrastructure''s gatekeeping role but could remove methodological check against community claims that are locally validated but empirically false. Each reading redistributes who counts as beneficiary and victim; none is bias-free.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coproduction_kernel_reading_choice, preference, 'Committer-axis: which reading of the legitimate-knowledge-boundary kernel is correct is a live three-way normative dispute, not an empirical question this story can resolve.').

omega_variable(
    coproduction_infrastructure_capture,
    'Has the co-production requirement been substantially captured by the intermediary organizations and program directors whose professional existence depends on it, independent of whether integration remains the best epistemic strategy for any given knowledge domain?',
    'Compare fields/domains where co-production requirements were introduced then later relaxed or made optional (if any) against fields where the requirement has hardened into mandatory gatekeeping criterion for funding and publication; track whether intermediary organization budgets/headcounts grew faster than measurable improvements in policy uptake or community outcomes.',
    'If capture is substantial, the standard should be reclassified toward the snare end of tangled_rope (or split into a Piton reading for fields where the founding problem has become dead but the requirement persists institutionally); if capture is minor, the tangled_rope classification with moderate extraction stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coproduction_infrastructure_capture, empirical, 'Whether rising theater_ratio and extraction reflect genuine institutional capture of the co-production apparatus.').

omega_variable(
    integration_vs_subordination_ambiguity,
    'When co-production ''integrates'' methodological and experiential validation, does it treat them as genuinely co-equal, or does experiential knowledge still require eventual methodological ratification to count — making ''integration'' a euphemism for continued subordination under a pluralistic label?',
    'Examine co-production case studies for instances where experiential/community findings that contradicted methodological findings were adopted as legitimate on their own terms, versus instances where such contradictions were resolved by requiring the experiential claim to be re-validated methodologically before acceptance.',
    'If methodological ratification is consistently the final arbiter, the experiential_pluralism_advocates'' objection is structurally correct and this reading''s claimed symmetry between the two validation modes is a false summit; if experiential findings are sometimes accepted over methodological objections, genuine integration is occurring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_subordination_ambiguity, conceptual, 'Whether the hybrid standard''s claimed symmetry between rigor and experience is real or whether methodology retains silent veto power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t4, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(legi_tr_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legi_be_t4, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(legi_be_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(legi_su_t4, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(legi_su_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(legi_su_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 24, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.1).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimate_knowledge_boundary kernel, each instantiated as its own story with its own ε, beneficiary/victim structure, and claimed type per the ε-invariance principle. The credentialed_expertise_reading (methodological rigor alone) is expected to show lower extraction and a rope or mountain-adjacent profile from the credentialed-institution seat, with victims among excluded community knowledge holders. The experiential_pluralism_reading (community validation as sufficient) is expected to show a different beneficiary/victim structure centered on community-institution power dynamics, potentially with lower formal-infrastructure extraction but its own exclusions (e.g., of claims lacking community consensus). This hybrid_coproduction_reading is the intermediate, infrastructure-heavy reading: it does not average the other two but introduces a distinct dual-gate mechanism and a distinct set of beneficiaries (boundary organizations, co-production program directors) that neither sibling reading's structure produces. All three should be linked bidirectionally in the network graph as siblings of the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
