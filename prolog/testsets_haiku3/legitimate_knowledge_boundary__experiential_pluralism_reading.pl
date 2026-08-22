% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Reading of Legitimate Knowledge Boundary
 *   domain: epistemology/science_studies/political_theory
 *
 * SUMMARY:
 *   The experiential pluralism reading of the legitimate knowledge boundary
 *   asserts that knowledge arising from lived experience, embedded community
 *   practice, and distributed validation mechanisms constitutes legitimate
 *   knowledge co-equal to or exceeding in authority methodologically rigorous
 *   inquiry validated through credentialed peer review. This reading is one
 *   instantiation of a contested kernel—the foundational question of what
 *   makes knowledge legitimate—and directly contests the credentialed
 *   expertise reading's grounding of legitimacy in methodological standards
 *   and institutional gatekeeping. The reading operates as both a corrective
 *   to historical epistemic injustice AND as a mechanism that creates new
 *   structural asymmetries: it validates previously excluded communities
 *   while requiring institutional gatekeepers to cede authority, thus
 *   generating extraction costs borne by those whose power depended on
 *   maintaining methodological standards as the binding legitimacy criterion.
 *
 * KEY AGENTS:
 *   - Community knowledge holders: Indigenous knowledge keepers, patient advocates, environmental justice practitioners; gain epistemic authority; set validation criteria within their networks
 *   - Experiential researchers: Participatory action researchers, disability justice scholars, community-based scholars; gain methodological legitimacy without institutional intermediation
 *   - Credentialed gatekeepers: Universities, peer-reviewed journals, research institutions; lose gatekeeping authority and must share validation functions; face pressure on funding and prestige systems
 *   - Methodological purists: Philosophers of science, statisticians; see methodological standards degraded in status; bear reputational and disciplinary costs
 *   - Marginalized epistemic communities: Historically excluded communities (women's health, racialized traditions, occupational safety knowledge); gain recognition but compete for institutional resources
 *   - Funding institutions: Navigate between competing legitimacy claims; altered grant criteria under pressure from both readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.62).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.71).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Reading of Legitimate Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__experiential_pluralism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, 'a3cbd189-c397-4299-92ca-41783d65e175').
narrative_ontology:cs_kernel_codification('a3cbd189-c397-4299-92ca-41783d65e175', distributed).
narrative_ontology:cs_authority_grounding('a3cbd189-c397-4299-92ca-41783d65e175', distributed).
narrative_ontology:cs_reading_relation('a3cbd189-c397-4299-92ca-41783d65e175', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3cbd189-c397-4299-92ca-41783d65e175', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('a3cbd189-c397-4299-92ca-41783d65e175', foundational, lived_experience_epistemic_authority).
narrative_ontology:cs_axiom_status(lived_experience_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('a3cbd189-c397-4299-92ca-41783d65e175', lived_experience_epistemic_authority, deontological).
narrative_ontology:cs_axiom('a3cbd189-c397-4299-92ca-41783d65e175', foundational, distributed_community_validation_sufficiency).
narrative_ontology:cs_axiom_status(distributed_community_validation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('a3cbd189-c397-4299-92ca-41783d65e175', distributed_community_validation_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('a3cbd189-c397-4299-92ca-41783d65e175', secondary, methodological_standards_as_institutional_tools).
narrative_ontology:cs_axiom_status(methodological_standards_as_institutional_tools, holdable).
narrative_ontology:cs_axiom_grounding('a3cbd189-c397-4299-92ca-41783d65e175', methodological_standards_as_institutional_tools, empirically_contingent).
narrative_ontology:cs_reference_frame('a3cbd189-c397-4299-92ca-41783d65e175', epistemic_pluralism_with_community_authority).
narrative_ontology:cs_drift_state('a3cbd189-c397-4299-92ca-41783d65e175', contemporary_institutional_science_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a3cbd189-c397-4299-92ca-41783d65e175', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, community_knowledge_holders).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_researchers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_epistemic_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_institutional_gatekeepers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, methodological_purists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_epistemic_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, funding_institutions).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, epistemic_justice_doctrine).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge_principle).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_validation_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Indigenous knowledge keepers, patient advocacy groups, environmental justice practitioners, and other communities whose expertise derives from lived experience within specific contexts. Under this reading, their knowledge claims gain direct legitimacy without requiring translation into methodological frameworks. They participate in validation through community deliberation and direct evidence of effectiveness in their domains. They set agenda by asserting epistemic authority and building parallel validation networks.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_knowledge_holders, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, community_knowledge_holders, agenda_setter).

% Scholar-practitioners who integrate lived experience with research methods: participatory action researchers, community-based researchers, autoethnographers, disability justice scholars. This reading validates their work as legitimate knowledge production without requiring methodological deference to institutional peer review as the ultimate arbiter. They gain authority to set validation criteria within their communities.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_researchers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_researchers, agenda_setter).

% Communities whose knowledge has been systematically devalued or excluded by institutional science: women's health knowledge, racialized healing traditions, workers' occupational safety expertise. They benefit from the reading's assertion that their knowledge counts without needing institutional validation. They also bear costs when this reading competes with methodological standards for institutional resources, as funders and institutions navigate between competing legitimacy claims.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_epistemic_communities, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_epistemic_communities, payer).

% Universities, peer-reviewed journals, credentialing bodies, and research institutions whose authority has been grounded in monopolizing methodological validation. This reading requires them to cede gatekeeping authority and share validation functions with communities they previously adjudicated. Their institutional revenue streams and epistemic authority depend on maintaining methodological standards as the primary legitimacy filter.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_institutional_gatekeepers, payer,
    institutional, generational, mobile, global).

% Philosophers of science, statisticians, and disciplinary gatekeepers committed to rigorous methodology as the boundary condition for legitimate knowledge. They bear the cost of seeing methodological standards degraded in status to 'one tool among many' and of defending their standards against claims that they encode institutional power rather than epistemic virtue. Their professional identity and disciplinary authority rest on methodological rigor as non-negotiable.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, methodological_purists, payer,
    powerful, biographical, identity_locked, global).

% Government agencies, foundations, and corporate sponsors who allocate research funding. They navigate between supporting work meeting traditional methodological standards and supporting participatory/community-based research claiming equal legitimacy. The reading creates pressure on their grant criteria and creates contention over what counts as fundable knowledge production.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, funding_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, funding_institutions, payer).

% Scholars and practitioners who seek integration rather than either/or frameworks—those arguing for joint methodological rigor and experiential validity. Under the experiential pluralism reading, they are partially excluded: their insistence on methodological integration is read as insufficiently committed to experiential authority and as perpetuating credentialist gatekeeping under a reformist label. They would object that this reading replicates the either/or structure it claims to overcome.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, hybrid_coproduction_advocates, excluded,
    organized, biographical, constrained, global).

% Epistemic cartographers, science studies scholars, and philosophy of knowledge practitioners who analyze the boundary conditions, structural assumptions, and institutional consequences of different legitimacy frameworks. They inhabit no seat in the dispute itself but observe how each reading's operation distributes authority and alters what kinds of knowledge can be heard.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, analytical_epistemology_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, community_knowledge_holders).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the epistemic governance problem of how to recognize legitimate knowledge in contexts where communities have been systematically excluded from institutional validation processes. Establishes distributed, community-based validation mechanisms as co-equal to methodological peer review, enabling knowledge that emerges from practice, lived experience, and community deliberation to count without institutional intermediation.
% TRANSFER_FUNCTION: Moves epistemic authority from credentialed institutions and methodological gatekeepers to experiential communities and community validation networks. Redirects research funding, publication prestige, and policy influence away from exclusive credentialed channels toward participatory research and community knowledge systems. Transfers the burden of proof: communities no longer need to translate their knowledge into institutional methodological terms; institutional knowledge claims must now justify themselves to communities.
% ABSENT_VOICES: Hybrid coproduction advocates (who would argue the reading creates a false binary and preserves either/or thinking in a different form); methodological pluralists who see methodological standards as one form of distributed knowledge validation, not institutional gatekeeping; and practitioners of institutional science who see themselves as doing collaborative, participatory work but are read by the pluralism reading as fundamentally captured by credentialism. These constituencies would object that the binary framework obscures their own practice and forecloses integration.
% DISAPPEARANCE_RATIONALE: If this reading and its enforcement vanished, research funding and publication pathways would consolidate around credentialed peer review as the primary legitimacy gate; community knowledge holders would lose the institutional recognition and funding they have gained; participatory research programs would be defunded or absorbed into traditional methodological structures; and policy influence from marginalized communities would contract sharply. The knowledge economy would reorganize around institutional credentialism.
% FOUNDING_PROBLEM: Institutional science and credentialed peer review systematically devalue, exclude, and fail to recognize knowledge produced through lived experience, community practice, and non-institutional validation. This exclusion perpetuates epistemic injustice and causes material harm when communities' knowledge about their own contexts is dismissed or overridden by credentialed experts.
% FOUNDING_PROBLEM_CORROBORATION: Testified to by community knowledge holders, disability justice scholars, Indigenous researchers, patient-led research initiatives, and science studies scholars documenting historical epistemic injustice. Documented in participatory action research, standpoint epistemology scholarship, and health activism literature. Supported by demonstrable cases of institutional science harming communities whose knowledge was dismissed (medical racism, environmental racism, reproductive coercion). Corroboration from outside the beneficiary set comes from equity-oriented philosophy of science, institutional critics, and historians of science documenting the politics of credentialing.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.38 and plateaus around 0.62 by mid-interval, indicating that the reading's operation does perform genuine coordination (opening pathways for previously excluded knowledge) but also extracts from those whose authority depended on methodological gatekeeping. The coordination benefit is substantial (solving epistemic injustice) but asymmetrically distributed: beneficiaries gain authority at relatively low cost, while payers (credentialed institutions and methodological gatekeepers) bear real losses of prestige and influence. Theater ratio rises from 0.25 to 0.48, indicating that as the reading becomes established, an increasing share of activity goes to performative boundary maintenance—communities asserting experiential authority, institutions incorporating 'participatory' language while maintaining gatekeeping functions, funders announcing commitment to 'community-based knowledge' while resource allocation remains heavily credentialist. Suppression increases from 0.55 to 0.71, reflecting the active enforcement required to prevent methodological reabsorption: community knowledge holders must continuously defend against pressure to translate their knowledge into methodological terms; institutions must resist methodological pushback from credentialist constituencies. The plateau in extractiveness by t=30 suggests the reading has stabilized into a contested-but-durable equilibrium: neither fully displacing methodological gatekeeping nor being fully absorbed back into it, but rather creating parallel validation systems with ongoing structural tension.
 *
 * PERSPECTIVAL GAP:
 *   The credentialed seat and the community-knowledge seat compute radically different types from identical structural data. The credentialed seat experiences the reading as a snare: extraction disguised as justice, enforcement requiring constant defense of standards against anti-intellectual pressure. The community-knowledge seat experiences it as rope: genuine coordination (finally being heard) with minimal overhead (community validation is costless relative to institutional review). The methodological-purist seat experiences it as tangled (coordination benefit of opening knowledge pathways, but extraction cost of standards degradation). The hybrid-coproduction seat experiences it as most extractive: excluded from both camps, benefits neither from institutional prestige nor from community autonomy, forced to argue against both sides simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Community knowledge holders, experiential researchers, and marginalized epistemic communities have low directionality (near beneficiary end): they gain epistemic authority, recognition, and funding pathways; their exit options are mobile (they can maintain community validation networks independently of institutional recognition, though institutional support accelerates their work). Credentialed gatekeepers and methodological purists have high directionality (near target end): they lose gatekeeping authority, see their disciplinary standards downgraded, and bear institutional and reputational costs; their exit options are mobile but their institutional power depends on maintaining methodological gatekeeping, so the functional exit cost is high. Funding institutions sit near symmetric: they benefit from expanded research partnerships and equity claims, but bear costs in managing contention between competing legitimacy frameworks. Hybrid coproduction advocates are partially trapped: they are excluded from the beneficiary set because their insistence on integration is read as insufficiently committed to experiential authority, but they are not fully in the payer seat because they also benefit from relaxed gatekeeping. This asymmetry (trapped between roles, excluded by the dominant reading while committed to partial justice) should compute as constrained exit and moderate-to-high directionality. The methodological purists carry identity_locked exit: their professional identity is constituted through commitment to methodological standards as non-negotiable epistemic virtue; exit would require dissolving the frame through which they understand their own expertise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epistemic injustice) is LIVE and well-corroborated from outside beneficiary seats. The disappearance verdict is world_rearranges: the constraint's removal would reorganize research funding, publication prestige, and policy influence systems. These facts prevent misclassification as piton (would require the founding problem to be dead and world_unchanged). The constraint is not pure rope because extraction is asymmetric and requires active enforcement: institutions must continuously resist methodological reabsorption, communities must continuously defend against pressure to legitimize their knowledge through methodological translation. This asymmetry and enforcement requirement distinguish it from rope, making tangled_rope the appropriate claim. The theater ratio rising to 0.48 indicates that performative activity increases as the constraint matures—institutions adopting 'participatory' language while maintaining gatekeeping functions, funders announcing 'community engagement' while allocation remains credentialist—but theater does not approach the 0.65+ that would signal piton-level theatricality. The theater tracks the constraint's actual operation: as it becomes established, the boundary between genuine coordination and strategic boundary-maintenance becomes harder to discern, but both functions remain present. The mandatrophy question (is the constraint's mandate obsolete?) is not live here: the founding problem persists, the constraint's coordinating function remains operative, and the extraction reflects structural asymmetry in how the reading distributes epistemic authority rather than mandate decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    experiential_authority_definition_ambiguity,
    'What precisely constitutes ''lived experience'' as a legitimate knowledge basis? Does it include all claims grounded in personal or community practice, or are there epistemic boundaries within experiential authority itself?',
    'Longitudinal study of which experiential claims actually influence policy and practice outcomes; documentary analysis of how communities themselves adjudicate validity within experiential knowledge frameworks; case comparison of failed vs. successful community-knowledge interventions.',
    'If lived experience has no internal boundaries, the reading risks absorbing non-verifiable or even harmful practices into ''legitimate knowledge'' (e.g., practices causing medical harm but defended as community tradition). If boundaries exist but are not methodologically specified, the reading risks replicating gatekeeping under a different name (community elders become the new credentialed gatekeepers). The classification depends on whether distributed validation actually produces reliability or merely redistributes who decides.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(experiential_authority_definition_ambiguity, empirical, 'Whether lived experience is the legitimacy criterion or lived experience is subject to its own validity standards.').

omega_variable(
    institutional_capture_of_participatory_language,
    'To what extent do institutions rhetorically adopt ''participatory'' and ''community engagement'' language while maintaining credentialist gatekeeping functions (resource allocation, publication prestige, policy influence)? Is theater ratio tracking genuine co-production or strategic boundary-maintenance?',
    'Audit of funding flow, publication prestige allocation, and policy influence channels over time: do community-sourced knowledge actually gain equivalent prestige and resource access, or does institutional language shift while allocation remains credentialist? Trace the career trajectories of community-based researchers: do they gain institutional recognition without requiring credential translation?',
    'High institutional capture would mean the constraint is primarily extractive (extract legitimacy narrative from communities while maintaining control), pushing classification toward snare. Low capture would confirm tangled_rope: genuine coordination (opening pathways) with asymmetric extraction (credentialists lose authority). The theater ratio serves as a leading indicator; measurement plateau or decline would suggest capture has stabilized; continued rise would suggest increasing performative activity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_participatory_language, empirical, 'Whether institutions genuinely share epistemic authority or adopt participatory language as a legitimacy capture mechanism.').

omega_variable(
    methodological_standards_as_power_or_epistemic_virtue,
    'Are methodological standards fundamentally tools of institutional power and gatekeeping (as the experiential pluralism reading frames them) or are they genuine epistemic virtues developed through long struggle to identify reliable knowledge (as the credentialed expertise reading claims)?',
    'Historical and philosophical analysis: trace the development of methodological standards alongside their use for gatekeeping and exclusion; distinguish the epistemic content from the institutional apparatus. Comparative case analysis of rigorous community-based research and methodologically-credentialed-but-contextually-wrong research; examine instances where methodological rigor prevented harm vs. instances where methodological requirements masked institutional gatekeeping.',
    'If methodological standards are primarily tools of power, the experiential pluralism reading''s challenge to methodological supremacy is justice work. If standards are genuine epistemic virtues, the reading risks abandoning tools that protect against harm and error. This is a reading-indexical question: what counts as evidence of ''genuine virtue'' is itself contested between the readings. The impact on classification is a reclassification between readings (credentialed vs. experiential) rather than a shift within this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(methodological_standards_as_power_or_epistemic_virtue, conceptual, 'The foundational framing dispute: are methodological standards epistemic tools or instruments of institutional power?').

omega_variable(
    exclusion_of_hybrid_coproduction,
    'Does the experiential pluralism reading necessarily exclude or marginalize hybrid coproduction approaches, or does this exclusion arise from how the reading is enforced in practice by particular communities or institutions?',
    'Examine whether hybrid approaches gain recognition within community-knowledge movements and whether the experiential pluralism reading as theoretically stated requires exclusion of integration or whether enforcement actors impose exclusion. Compare institutional support for hybrid coproduction vs. methodologically-pure vs. experientially-pure research programs.',
    'If the reading necessarily forecloses hybrid work, the boundary dispute is more zero-sum and the reading''s claim to justice is weaker (excludes those seeking integration). If exclusion is contingent to enforcement and integration is theoretically possible within the reading, the reading is less extractive and more genuinely coordinating. This shapes whether the constraint''s asymmetry is intrinsic to the reading or contingent to its implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_of_hybrid_coproduction, conceptual, 'Whether the experiential pluralism reading logically excludes hybrid coproduction or whether exclusion is contingent to enforcement practices.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of a contested kernel (legitimate_knowledge_boundary). The reading is instantiated through specific axioms and authority claims. If those axioms are challenged by evidence (axiom_overriding drift), does the reading logically foreclose or merely weaken?',
    'Monitor drift_state for the experiential_pluralism_reading: empirical challenges to the axiom (do communities'' knowledge systems actually produce better outcomes than methodological approaches?), authority erosion (do institutions and funding systems continue to recognize community knowledge as legitimate?), or practice_drift (do practitioners integrate methodology despite the reading''s claim that methodology is subordinate?). The engine computes whether drift forces reclassification between readings or merely signals within-reading pressure.',
    'If empirical challenges accumulate (axiom_overriding drift, severe, acknowledged), the reading''s viability shifts. The committer frame allows this to surface as a reading-level question rather than embedding it in constraint metrics. The classification stays within this reading''s lens; the measurement of the reading''s own stability is separate from the constraint''s per-seat classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, empirical, 'How the experiential pluralism reading responds to evidence about knowledge outcomes and community validation efficacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(legi_tr_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement(legi_tr_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(legi_tr_t40, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(legi_be_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(legi_be_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(legi_be_t40, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(legi_su_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(legi_su_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(legi_su_t40, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% The legitimate_knowledge_boundary kernel is instantiated across three constraint stories, each representing a different reading of the foundational epistemic legitimacy question. The experiential_pluralism_reading instantiates the position that lived experience and community validation ground legitimate knowledge. The credentialed_expertise_reading instantiates the position that methodological rigor and credentialed peer review ground legitimacy. The hybrid_coproduction_reading seeks integration of both. These are not alternative measurements of a single constraint; they are structurally distinct constraints that share a contested kernel. Each reading has different beneficiary/victim structures, different enforcement requirements, and different ε values because each reading defines the referent (the standing arrangement under contest) differently. The stories are linked via affects_constraints to enable contamination propagation analysis and to document the constraint family structure. A change in one reading's stability (e.g., empirical challenges to community validation's efficacy) affects the structure and viability of the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__experiential_pluralism_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
