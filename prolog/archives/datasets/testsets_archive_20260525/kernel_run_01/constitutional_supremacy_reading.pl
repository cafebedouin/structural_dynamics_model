% ============================================================================
% CONSTRAINT STORY: constitutional_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_supremacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_supremacy_reading
 *   human_readable: Constitutional Supremacy Reading: Family Law Authority and Secular Equality
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   The constitutional supremacy reading of family law authority holds that
 *   legitimate governance of family law derives from constitutional mandates
 *   for equality and secularism, implemented through Article 44 Uniform Civil
 *   Code (UCC) directives or equivalent constitutional provisions. Personal
 *   laws — religious, customary, or community-based family law frameworks —
 *   are treated as state-regulated constructs subordinate to fundamental
 *   rights review. This reading prioritizes individual rights-holders over
 *   community identity claims and positions secular, egalitarian law as the
 *   constitutional endpoint. The constraint exhibits genuine tangled-rope
 *   structure: constitutional supremacy coordinates the modernization of
 *   family law (moving from discriminatory personal law provisions toward
 *   equal treatment) while simultaneously extracting legitimacy from personal
 *   law communities and foreclosing their own authority systems. The reading
 *   is ONE instantiation of a contested kernel (family_law_authority) that
 *   admits at least three structurally distinct readings: this constitutional
 *   supremacy version, a religious autonomy reading (which grounds family law
 *   authority in community religious traditions), and a pluralist
 *   accommodation reading (which coordinates multiple legal systems without
 *   hierarchy). The perspectives show how these readings have radically
 *   different experienced extractiveness depending on institutional position:
 *   personal law community members experience snare-level constraints;
 *   constitutional courts experience coordination; civil rights advocates see
 *   a justified sunset structure; the personal law state apparatus has
 *   degraded into a performative piton. The analytical observer risks
 *   naturalizing the constitutional supremacy framework as inevitable rather
 *   than contingent.
 *
 * KEY AGENTS:
 *   - Individual rights-holders (especially women): Primary beneficiaries (analytical/mobile) — gain equality protections, legal recourse against discriminatory personal law provisions, freedom from community enforcement of family obligations.
 *   - Personal law communities: Primary victims (powerless/trapped) — lose authority to govern their own family law, forced conformity to secular standards, community identity authority subordinated.
 *   - Constitutional courts: Institutional beneficiary (institutional/arbitrage) — concentration of adjudicatory power, expanded jurisdiction over family law disputes, doctrinal authority to interpret and constrain personal law provisions.
 *   - Reform movement insiders: Mixed position (moderate/identity_locked) — benefit from equality protections but bear costs of internal translation work and community social penalties.
 *   - Civil rights advocacy organizations: Organized agents (organized/constrained) — coordinate toward uniform civil law endpoint; see constitutional supremacy as justified sunset structure.
 *   - Personal law state apparatus: Degraded institution (institutional/constrained) — maintains parallel systems while losing legitimacy, increasing performative function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_supremacy_reading, 0.52).
domain_priors:suppression_score(constitutional_supremacy_reading, 0.68).
domain_priors:theater_ratio(constitutional_supremacy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_supremacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_supremacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_supremacy_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_supremacy_reading, "Constitutional Supremacy Reading: Family Law Authority and Secular Equality").
narrative_ontology:topic_domain(constitutional_supremacy_reading, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(constitutional_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(constitutional_supremacy_reading, formalized).
narrative_ontology:cs_authority_grounding(constitutional_supremacy_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(constitutional_supremacy_reading).
narrative_ontology:cs_kernel_id(constitutional_supremacy_reading, family_law_authority).
narrative_ontology:cs_reading_relation(constitutional_supremacy_reading, religious_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation(constitutional_supremacy_reading, pluralist_accommodation_reading, influences).
narrative_ontology:cs_axiom(constitutional_supremacy_reading, foundational, constitutional_equality_mandate).
narrative_ontology:cs_axiom_status(constitutional_equality_mandate, holdable).
narrative_ontology:cs_axiom(constitutional_supremacy_reading, foundational, secular_state_authority_supremacy).
narrative_ontology:cs_axiom_status(secular_state_authority_supremacy, holdable).
narrative_ontology:cs_reference_frame(constitutional_supremacy_reading, constitutional_equality_supremacy).
narrative_ontology:cs_drift_state(constitutional_supremacy_reading, contemporary_pluralist_challenge, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_supremacy_reading, individual_rights_holders).
narrative_ontology:constraint_beneficiary(constitutional_supremacy_reading, constitutional_court_authority).
narrative_ontology:constraint_beneficiary(constitutional_supremacy_reading, secular_law_framework).
narrative_ontology:constraint_victim(constitutional_supremacy_reading, personal_law_communities).
narrative_ontology:constraint_victim(constitutional_supremacy_reading, religious_autonomy_claims).
narrative_ontology:constraint_victim(constitutional_supremacy_reading, cultural_minority_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERSONAL LAW COMMUNITY MEMBERS (SNARE) — Structurally trapped. Exit from personal law frameworks requires abandoning community membership, family recognition, and identity. No genuine alternatives available within their cultural context. The constitutional supremacy reading forecloses their own authority systems as legitimate, forcing conformity to secular equality standards while denying the coordination function those personal laws provide within their communities. High experienced extraction with minimal escape routes.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM-MOVEMENT INSIDERS (TANGLED ROPE) — Identity-locked within both personal law traditions AND constitutional equality frameworks. Benefit from constitutional protections of individual choice (especially women's inheritance, divorce rights, guardianship). Simultaneously bear the cost of internal community delegation — often female family law reformers must navigate both systems, translate between frameworks, and face social penalties for rights assertions. The constraint coordinates modernization of personal law while extracting cultural legitimacy from reform advocates. Moderate agent power with constrained-to-identity-locked exit options.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONSTITUTIONAL COURT AUTHORITY (ROPE) — Institutional beneficiary. The constitutional supremacy reading concentrates adjudicatory power in constitutional courts: personal law disputes are re-framed as constitutional rights questions, expanding court jurisdiction and authority. Courts coordinate the simultaneous application of constitutional principles and personal law provisions through doctrine (reading down, progressive interpretation, essential features doctrine). The constraint appears as pure coordination from this perspective — enforcing constitutional supremacy is the court's assigned function. Minimal experienced extraction; substantial institutional benefit.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS ADVOCACY ORGANIZATIONS (SCAFFOLD) — Organized agents seeking transitional convergence toward uniform civil law. See the constitutional supremacy reading as a sunset structure: individual rights protections gradually displace personal law community authority, with the endpoint being a secular, egalitarian family law framework replacing all personal law provisions. Theater is relatively low — advocacy focuses on substantive rights (divorce, inheritance, guardianship) rather than performative constitutional ritual. High suppression of competing legal frameworks is tolerated because the sunset is perceived as inevitable and just.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PERSONAL LAW STATE APPARATUS (PITON) — Degraded institutional structure. The state maintains parallel personal law administration (religious courts, community councils, state-regulated personal law codes) while simultaneously subordinating them to constitutional review. The personal law system persists through inertia and political compromise despite loss of authority legitimacy. Theater ratio is moderate-to-high: the state performs commitment to 'accommodating diversity' while progressively constraining personal law discretion through constitutional doctrine. The apparatus has become largely performative — it exists to manage the fiction of legal pluralism while actual power flows to constitutional courts.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the universal principle of equality before law and secular adjudication appears as an immutable feature of modern constitutional democracies — a natural law of legitimate state authority that transcends cultural particularity. However, this perspective risks a false summit: the universal claim naturalizes a specific historical configuration (European-derived constitutional secularism) and treats contingent institutional choices (constitutional courts as supreme arbiters) as structural necessities. The mountain classification reveals how dominant frameworks naturalize their own contingency.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_supremacy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_supremacy_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constitutional supremacy reading extracts from personal law communities by subordinating their authority systems to constitutional review while refusing to recognize the coordination function those systems provide. However, extraction is not maximal (0.7+) because the constraint also provides genuine benefits to individual rights-holders (especially protections against discriminatory personal law rules). The measurement trajectory shows increasing extractiveness from 0.35 (early constitutional periods with stronger personal law autonomy) to 0.52 (contemporary constitutional doctrine with progressively stricter fundamental rights review). Suppression (0.68): High. Significant barriers prevent personal law communities from maintaining their authority systems: constitutional courts override community decisions, legislative mandates impose secular requirements, interpretation doctrine systematically privileges constitutional principles, and dissenting individuals can exit to secular law (creating pressure on community unity). Theater ratio (0.35): Low-to-moderate. Unlike many institutional structures, the constitutional supremacy reading relies relatively little on performative ritual. The actual work is doctrinal and substantive: constitutional courts genuinely apply fundamental rights standards to personal law provisions, producing real redistribution of authority (not theater). Theater appears in the state's continued maintenance of personal law systems despite loss of legitimacy — the appearance of accommodation performs legal pluralism while actual power flows elsewhere.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence from a single constitutional structure. Personal law community members (powerless/trapped) see pure extraction (snare) — their authority forecloses with no gain. Constitutional courts (institutional/arbitrage) see pure coordination (rope) — enforcing supremacy is their legitimate function. Civil rights advocates (organized/constrained) see a justified temporary constraint with a sunset (scaffold) — uniform law is the endpoint. The personal law state (institutional/constrained) sees its own degradation (piton) — parallel systems persist through inertia despite loss of functional legitimacy. Reform insiders (moderate/identity_locked) see mixed coordination and extraction (tangled_rope) — gaining equality protections while losing community standing. The analytical observer at civilizational scope risks classifying as mountain — naturalizing constitutional supremacy as an inevitable principle of legitimate democracy. The perspectival gaps reveal that this is not a dispute about facts but about which institutional values (individual equality vs community autonomy, secular governance vs religious authority, state hierarchy vs legal pluralism) take precedence. Different readings privilege different values.
 *
 * DIRECTIONALITY LOGIC:
 *   The constitutional supremacy reading's directionality structure is complex because it operates through multiple institutional channels. For individual rights-holders (beneficiaries with mobile/analytical exit): d is low (0.15-0.25) — they benefit from constitutional protections and can access secular law alternatives without massive cost. They experience negative or minimal extraction. For personal law communities (victims with trapped exit): d is high (0.85-0.95) — they cannot exit without ceasing to be communities; constitutional supremacy forecloses their authority system; they bear full extraction cost. For constitutional courts (beneficiary institutions with arbitrage exit): d is low (0.10-0.20) — they benefit from jurisdictional expansion and doctrinal authority; they have complete exit optionality (their institutional survival does not depend on personal law systems). The reform-movement insiders occupy a middle position: identity_locked exit means they cannot escape the constraint without abandoning their commitment to both personal law tradition AND constitutional equality — making their d moderate-high (0.60-0.70) despite moderate agent power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equality_standard_incompatibility,
    'Is the incompatibility between constitutional equality principles and personal law community discretion logically irreducible or contingent on how equality is defined?',
    'Comparative analysis of alternative equality frameworks: substantive vs formal equality, group-differentiated vs individual-rights equality, cultural preservation as equality dimension. Test whether personal law provisions can be reformulated to satisfy modified equality standards without elimination.',
    'If logically irreducible: constitutional supremacy forecloses religious autonomy reading within any single framework — classification moves toward genuine logical precedence. If contingent: the relationship downgrades to coexistence or influence — readings remain live options under different definitional choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equality_standard_incompatibility, conceptual, 'Whether equality incompatibility with personal law is logical or definitional').

omega_variable(
    individual_exit_capacity_empirical,
    'How many personal law community members are genuinely capable of exiting to secular law frameworks without catastrophic loss of relational, economic, or identity security?',
    'Empirical measurement: exit rate analysis (who actually converts family law framework), cost trajectory (economic/relational penalties), alternative availability (whether secular law provides equivalent protections for non-conforming actors).',
    'If exit capacity is high: ''trapped'' classification for community members downgrades to ''constrained'' — extraction severity reduces. If exit capacity remains low: suppression metric may need upward revision; the constraint is more snare-like than tangled-rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_exit_capacity_empirical, empirical, 'Empirical exit capacity from personal law frameworks to secular alternatives').

omega_variable(
    constitutional_court_neutrality_assumption,
    'Do constitutional courts genuinely exercise neutral arbitrage between constitutional and personal law principles, or do they systematically privilege constitutional supremacy through interpretive doctrine?',
    'Doctrinal analysis: comparative study of how courts resolve conflicts (precedence rules, hierarchy of norms, burden of proof allocation). Test whether courts ever subordinate constitutional claims to personal law values or whether subordination flows only one direction.',
    'If genuinely neutral: tangled_rope classification is accurate (mixed coordination/extraction). If systematically biased toward constitutional supremacy: extraction component increases; relationship downgrades toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_court_neutrality_assumption, empirical, 'Whether constitutional courts exercise genuine neutrality between constitutional and personal law principles').

omega_variable(
    reading_identity_vs_charter_identity,
    'Is the constitutional supremacy reading grounded in the actual constitutional text and charter-founding commitments, or does it represent a retrospective imposition of a secular-equality ideology that the founding generations did not clearly endorse?',
    'Textual and historical analysis: examine constitutional language on personal law provisions, founding-generation debates, subsequent amendments. Test whether constitutional supremacy can be derived from text alone or requires ideological interpolation.',
    'If grounded in text: reading''s authority claim is strongest; axiom ''constitutional_equality_mandate'' remains holdable. If requiring interpolation: axiom status may be ''overridden'' if founding text explicitly protected personal law domains; if interpolation conflicts with text, status becomes ''foreclosed''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_vs_charter_identity, empirical, 'Whether constitutional supremacy is textually grounded or ideologically imposed').

omega_variable(
    kernel_committer_reading_ambiguity,
    'This constraint is ONE reading of the family_law_authority kernel. Does this reading foreclose the religious_autonomy_reading and pluralist_accommodation_reading, or do all three coexist as live institutional options?',
    'Institutional analysis: examine whether adopting constitutional supremacy reading within a single legal system logically prevents courts, legislatures, or communities from also holding religious autonomy or pluralist readings. Test whether the readings can be held simultaneously by different institutional actors or whether one actor''s adoption of constitutional supremacy forces foreclosure of alternatives.',
    'If foreclosure is genuine: reading_relations declare forecloses; axioms remain holdable. If coexistence is structural: reading_relations declare coexists_with; different institutional populations hold competing readings simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_reading_ambiguity, conceptual, 'Whether this reading logically forecloses or coexists with sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_supremacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_supremacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cons_tr_t10, constitutional_supremacy_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(cons_tr_t20, constitutional_supremacy_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_supremacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t10, constitutional_supremacy_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cons_be_t20, constitutional_supremacy_reading, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_supremacy_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_supremacy_reading, religious_autonomy_reading).
narrative_ontology:affects_constraint(constitutional_supremacy_reading, pluralist_accommodation_reading).
narrative_ontology:affects_constraint(constitutional_supremacy_reading, uniform_civil_code_implementation).
narrative_ontology:affects_constraint(constitutional_supremacy_reading, personal_law_community_exit).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel admits multiple readings. This story models ONLY the constitutional supremacy reading. The religious_autonomy_reading and pluralist_accommodation_reading are separate constraint stories with different ε values, different beneficiary/victim structures, and different classifications. All three readings operate on the SAME social reality but instantiate different constraints through different interpretive frameworks. They are linked via reading_relations in cs_structure, not via affects_constraints in network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_supremacy_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
