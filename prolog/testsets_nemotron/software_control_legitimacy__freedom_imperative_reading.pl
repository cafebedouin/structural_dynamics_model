% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Freedom-Imperative Reading of Software Control Legitimacy
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   The freedom-imperative reading of software control legitimacy —
 *   associated with the Free Software Foundation and the GNU project —
 *   asserts that proprietary software is ethically illegitimate because it
 *   denies users the four essential freedoms (run, study, modify,
 *   distribute). This reading instantiates a constraint that categorically
 *   rejects any software that does not grant these freedoms, treating all
 *   proprietary software as a violation of user rights. The constraint
 *   operates through copyleft licensing (GPL family) which uses copyright law
 *   to make freedom inalienable: derivatives must carry the same freedoms.
 *   The reading's extraction is high because it demands the surrender of
 *   proprietary business models; its suppression is high because it treats
 *   compromise as moral failure and structurally excludes users who depend on
 *   proprietary tools.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.82).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.75).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Freedom-Imperative Reading of Software Control Legitimacy").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, 'a71d35e8-2dfc-48e6-b826-8bf274b88957').
narrative_ontology:cs_kernel_codification('a71d35e8-2dfc-48e6-b826-8bf274b88957', formalized).
narrative_ontology:cs_authority_grounding('a71d35e8-2dfc-48e6-b826-8bf274b88957', lineage).
narrative_ontology:cs_interpretation_layer_present('a71d35e8-2dfc-48e6-b826-8bf274b88957').
narrative_ontology:cs_reading_relation('a71d35e8-2dfc-48e6-b826-8bf274b88957', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('a71d35e8-2dfc-48e6-b826-8bf274b88957', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('a71d35e8-2dfc-48e6-b826-8bf274b88957', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('a71d35e8-2dfc-48e6-b826-8bf274b88957', foundational, proprietary_software_categorically_illegitimate).
narrative_ontology:cs_axiom_status(proprietary_software_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('a71d35e8-2dfc-48e6-b826-8bf274b88957', proprietary_software_categorically_illegitimate, deontological).
narrative_ontology:cs_axiom('a71d35e8-2dfc-48e6-b826-8bf274b88957', foundational, user_freedom_inalienable_via_copyleft).
narrative_ontology:cs_axiom_status(user_freedom_inalienable_via_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('a71d35e8-2dfc-48e6-b826-8bf274b88957', user_freedom_inalienable_via_copyleft, conventional).
narrative_ontology:cs_reference_frame('a71d35e8-2dfc-48e6-b826-8bf274b88957', hacker_ethic_shared_source_norm).
narrative_ontology:cs_drift_state('a71d35e8-2dfc-48e6-b826-8bf274b88957', cloud_saas_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a71d35e8-2dfc-48e6-b826-8bf274b88957', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, software_users_as_rights_holders).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, free_software_advocates).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, copyleft_communities).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, commercial_software_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, users_dependent_on_proprietary_ecosystems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users whose computing freedom is structurally denied by proprietary software; they benefit from the freedom-imperative reading because it names their exclusion as injustice and provides a moral framework for demanding source access, modification rights, and redistribution freedom. Their exit from proprietary ecosystems is identity-locked because adopting this reading fuses their self-concept with the freedom struggle — leaving the reading means abandoning the moral identity.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, software_users_as_rights_holders, beneficiary,
    organized, biographical, identity_locked, global).

% Activists and organizations (FSF, FSFE, Software Freedom Conservancy) who set the agenda for the freedom-imperative reading. They author the licenses (GPL family), define the boundary of 'free', and wage the cultural and legal campaigns. They benefit from the reading's moral authority and institutional recognition. Their professional and personal identities are fused with the reading — exit would mean abandoning life's work.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, beneficiary).

% Developer communities building and maintaining copyleft-licensed software ecosystems. They benefit from the reading's normative force which protects their work from proprietary enclosure and recruits contributors. Exit is constrained — they could switch to permissive licensing but the copyleft commitment is structural to their project governance and community trust.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, copyleft_communities, beneficiary,
    organized, biographical, constrained, global).

% Companies whose business model depends on restricting user control (source closure, license enforcement, SaaS lock-in). They bear the extraction of the freedom-imperative reading: moral delegitimization, legal pressure from copyleft compliance, competitive disadvantage from 'free' alternatives, and regulatory risk when freedom rhetoric enters policy. Exit is constrained — they could open-source but their capital structure and investor expectations lock the proprietary model.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, payer,
    institutional, biographical, constrained, global).

% Individual developers and small firms building proprietary software. They pay through restricted career mobility (copyleft communities may shun proprietary work), moral injury from freedom-advocate critique, and legal exposure when their code interacts with copyleft dependencies. Exit is constrained — skills are transferable but the proprietary software economy is where the paid work concentrates.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, commercial_software_developers, payer,
    moderate, biographical, constrained, global).

% Users who rely on proprietary software for accessibility, specialized professional tools, hardware drivers, or regulatory compliance. The freedom-imperative reading structurally victimizes them by declaring their necessary tools illegitimate without providing viable free alternatives. They are trapped — they cannot exit the proprietary ecosystem without losing essential functionality, and the reading offers no transitional scaffold.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, users_dependent_on_proprietary_ecosystems, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, users_dependent_on_proprietary_ecosystems, payer).

% Developers who contribute to open source for practical benefits (peer review, collaboration, talent recruitment) but reject the freedom-imperative's moral absolutism. They would object to being conscripted into a freedom struggle they see as counterproductive. Their exit is mobile — they can ignore the reading and continue building on permissive licenses.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, pragmatic_open_source_developers, excluded,
    organized, biographical, mobile, global).

% Academic and policy researchers studying software freedom, intellectual property, and digital autonomy. They analyze the reading's claims, track its policy influence, and document its empirical effects on innovation and user welfare. They neither collect nor pay.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, digital_rights_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global moral and legal framework that treats user computing freedom as non-negotiable, enabling copyleft licensing to function as a self-enforcing commons that prevents enclosure of shared code.
% TRANSFER_FUNCTION: Transfers moral legitimacy and legal enforceability from proprietary software vendors to users-as-rights-holders; transfers development effort from proprietary capture to copyleft commons; transfers the burden of proof onto anyone restricting user control.
% ABSENT_VOICES: Users in the Global South dependent on proprietary mobile ecosystems for basic connectivity; accessibility users locked into proprietary assistive technology; regulators who see the reading's absolutism as incompatible with security certification requirements. These voices are structurally excluded because the reading's moral framework admits no legitimate exceptions.
% DISAPPEARANCE_RATIONALE: If the freedom-imperative reading vanished overnight, copyleft's moral foundation would collapse — GPL enforcement would lose its ethical force, proprietary vendors would face no principled opposition to enclosure, and the 'free software' identity would dissolve into 'open source' pragmatism. The global commons of copyleft code would become legally vulnerable to proprietary relicensing.
% FOUNDING_PROBLEM: Proprietary software in the 1980s increasingly restricted users from studying, modifying, and sharing the programs they ran — a shift from the hacker norm of shared source to commercial enclosure of code. The freedom-imperative reading was built to name this enclosure as injustice and construct a legal-moral counter-mechanism (copyleft) that would make user freedom inalienable.
% FOUNDING_PROBLEM_CORROBORATION: Free software advocates (Stallman, FSF) attest the problem is live and worsening (SaaS, IoT, hardware lockdowns). Pragmatic open source proponents (OSI, corporate open source offices) attest the founding problem is substantially solved for most users — source availability and collaborative development won; the freedom frame is now counterproductive. Independent historians of computing (e.g. Coleman, Kelty) corroborate that the original enclosure dynamic was real but argue the reading's absolutism fails to account for the complex ecology of modern software production where proprietary and free code are deeply interdependent.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.82) reflects the reading's categorical demand: all proprietary software must become free or cease to exist — a total claim on the software economy. Suppression (0.75) is high because the reading's persistence depends on treating proprietary alternatives as illegitimate rather than competing with them; users_dependent_on_proprietary_ecosystems are trapped victims. Theater ratio (0.15) is low because the reading's enforcement (GPL compliance, advocacy) is functionally aligned with its stated goal — the performance is the substance. Accessibility collapse (0.65) is moderate-high because once the freedom frame is adopted, proprietary software appears as injustice rather than choice. Resistance (0.7) is high because the proprietary software industry, pragmatic open source, and trapped users all resist the reading's totalizing claim.
 *
 * PERSPECTIVAL GAP:
 *   From the free_software_advocate seat, the constraint is a rope — genuine coordination of a moral commons that prevents enclosure. From the proprietary_vendor seat, it is a snare — extraction of their business model under cover of morality. From the trapped_user seat, it is a snare with no coordination benefit — pure suppression of their necessary tools. The engine computes this divergence from the structural data; the authored claim (snare) reflects the aggregate structural reality where extraction and suppression dominate.
 *
 * DIRECTIONALITY LOGIC:
 *   software_users_as_rights_holders and free_software_advocates are structural beneficiaries (d ~ 0.15): the reading subsidizes their moral standing and legal tools. copyleft_communities are near-symmetric beneficiaries (d ~ 0.35): they gain protection but bear maintenance burden. proprietary_software_vendors and commercial_software_developers are targets (d ~ 0.85): they bear the full extraction of delegitimization and legal pressure. users_dependent_on_proprietary_ecosystems are hyper-targets (d ~ 0.95): trapped victims who pay the highest cost. pragmatic_open_source_developers are excluded (d ~ 0.6): they reject the frame but are pressured by its cultural dominance. digital_rights_scholars are analytical observers (d ~ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proprietary enclosure of 1980s software) was real and the reading's response (copyleft) was a genuine coordination innovation. But the reading has not adapted to the SaaS/cloud era where 'user control' means something fundamentally different than local binary modification. The reading's absolutism now extracts from users who need proprietary accessibility tools, medical device software, and regulated systems — a classic mandatrophy where the mandate (user freedom) has outlived its functional mechanism (copyleft on locally-run binaries). The reading persists through identity_locked advocates and institutional inertia (FSF, GPLv3) rather than functional fit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    saas_freedom_definition,
    'Does the freedom-imperative reading''s definition of ''user control'' meaningfully extend to Software-as-a-Service and cloud infrastructure where users never possess the binary?',
    'AGPL adoption metrics, FSF policy statements on SaaS, and whether copyleft communities actually treat cloud-hosted proprietary services as freedom violations equivalent to proprietary binaries.',
    'If the reading cannot coherently extend to SaaS, its extraction becomes performative — it extracts from traditional proprietary vendors while ignoring the dominant extraction mode of the cloud era, reclassifying toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_freedom_definition, conceptual, 'Whether the reading''s core concept of user control survives the SaaS/cloud transition.').

omega_variable(
    trapped_user_victimhood,
    'Are users_dependent_on_proprietary_ecosystems genuine victims of the freedom-imperative reading, or does the reading''s moral framework implicitly treat their dependence as a solvable problem (build free alternatives) rather than a structural trap?',
    'Track whether free software advocates allocate resources to building free replacements for the specific proprietary tools that trap accessibility users, medical device users, and Global South mobile users — or whether the reading''s rhetoric treats their dependence as moral compromise.',
    'If the reading structurally abandons trapped users, its victim set expands and its snare classification deepens; if it actively builds bridges, the coordination function strengthens and tangled_rope becomes plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trapped_user_victimhood, empirical, 'Whether the reading''s victim structure includes trapped users as genuine victims or rhetorical collateral.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to the software_control_legitimacy kernel differ from its siblings, and what specific structural elements do the readings disagree on?',
    'Compare the four readings'' beneficiary/victim sets, ε referents, and founding problem narratives. The disagreement is located on: (1) whether proprietary software per se enters the victim set (this reading: yes; pragmatic_openness: no; property_rights: proprietary vendors are beneficiaries); (2) whether user freedom is a fundamental right or a coordination preference; (3) whether the kernel''s referent is the proprietary software economy (this reading) or the open source development methodology (pragmatic_openness).',
    'Clarifies that the ε-invariance principle requires separate constraint stories for each reading — they have different ε referents, different victim sets, and different structural dynamics. The kernel is the contested label; the readings are the constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural delta between this reading and its siblings in the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 1983, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1983, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1983, 0.05).
narrative_ontology:measurement(soft_tr_t1991, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1991, 0.08).
narrative_ontology:measurement(soft_tr_t1998, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(soft_tr_t2007, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2007, 0.18).
narrative_ontology:measurement(soft_tr_t2015, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(soft_tr_t2026, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(soft_be_t1983, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1983, 0.35).
narrative_ontology:measurement(soft_be_t1991, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1991, 0.42).
narrative_ontology:measurement(soft_be_t1998, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(soft_be_t2007, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2007, 0.68).
narrative_ontology:measurement(soft_be_t2015, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(soft_be_t2026, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2026, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1983, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1983, 0.3).
narrative_ontology:measurement(soft_su_t1991, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1991, 0.4).
narrative_ontology:measurement(soft_su_t1998, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(soft_su_t2007, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2007, 0.68).
narrative_ontology:measurement(soft_su_t2015, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(soft_su_t2026, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2026, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__freedom_imperative_reading, 0.08).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, copyleft_license_enforcement).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_freedom_advocacy_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the software_control_legitimacy kernel. The freedom_imperative_reading treats proprietary software as categorically illegitimate (snare, high ε). The pragmatic_openness_reading treats open source as a superior methodology but proprietary as legitimate (rope, low ε). The property_rights_reading treats software control as a creator property right (mountain or rope, ε near 0). The commons_reading treats software as collectively governed infrastructure (tangled_rope, moderate ε). All four stories share the kernel_id and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__freedom_imperative_reading, organized, 0.15).
constraint_indexing:directionality_override(software_control_legitimacy__freedom_imperative_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
