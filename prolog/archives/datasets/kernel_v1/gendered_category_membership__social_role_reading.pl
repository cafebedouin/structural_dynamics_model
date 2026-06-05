% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gendered Category Membership via Sustained Social Performance (Social Role Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   The social role reading of gendered category membership grounds
 *   membership in sustained social performance and community recognition
 *   rather than biological facts or internal identity. Under this reading,
 *   'woman' is constituted through consistent performance of culturally
 *   recognized gender role practices, presentation, and social positioning —
 *   and crucially, through recognition by others who validate that
 *   performance. This reading produces a tangled hybrid constraint: it
 *   coordinates genuine social function (shared understanding of role-based
 *   norms enables collective action, mutual recognition, institutional
 *   coordination) while simultaneously extracting proof burdens from those
 *   seeking entry to the category. Trans women seeking recognition must
 *   perform at or above cis-female performance standards and secure
 *   recognition from gatekeepers distributed across social interactions. Cis
 *   women are conscripted into boundary-enforcement labor, constrained to
 *   police the category lest it become 'meaningless.' The constraint exhibits
 *   all six types depending on observer position: natural law for the
 *   civilizational analyst, piton for the bureaucratic institution
 *   maintaining outdated criteria, snare for the trans individual requiring
 *   recognition, tangled rope for the organized coalition, pure rope for the
 *   institutional allocator seeking functional clarity, and tangled rope
 *   again for the cis woman caught between solidarity and boundary policing.
 *
 * KEY AGENTS:
 *   - Trans women seeking recognition: primary target (powerless/identity_locked) — require continuous social performance and gatekeeping approval; identity fused with category membership
 *   - Cis women as boundary enforcers: secondary victim and enforcer (moderate/constrained) — perform coordination function (shared norms) but extracted into policing role; constrained exit because refusing to police risks accusations of category betrayal
 *   - Institutional role allocators: beneficiary (institutional/arbitrage) — can defer responsibility to 'objective' social role criteria while extracting conformity; gain institutional clarity and efficiency
 *   - Trans rights coalition: organized agent (organized/mobile) — see coordinate function but organized to shift reading or build alternative spaces; mobile enough to advocate alternative institutional arrangements
 *   - Legal/administrative classification systems: institutional actor (institutional/arbitrage) — maintain social role criteria through inertia despite increasing recognition of contest; performative criteria remain despite informal gatekeeping dominating actual recognition decisions
 *   - Analytical observer: civilizational position (analytical/analytical) — risks naturalizing contingent institutional gatekeeping as a feature of social ontology itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.38).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.52).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership via Sustained Social Performance (Social Role Reading)").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, 'e94d5696-c649-4bba-8a91-94697cd20f08').
narrative_ontology:cs_kernel_codification('e94d5696-c649-4bba-8a91-94697cd20f08', distributed).
narrative_ontology:cs_authority_grounding('e94d5696-c649-4bba-8a91-94697cd20f08', extraction).
narrative_ontology:cs_reading_relation('e94d5696-c649-4bba-8a91-94697cd20f08', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('e94d5696-c649-4bba-8a91-94697cd20f08', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('e94d5696-c649-4bba-8a91-94697cd20f08', foundational, category_membership_constituted_through_social_performance).
narrative_ontology:cs_axiom_status(category_membership_constituted_through_social_performance, holdable).
narrative_ontology:cs_axiom_grounding('e94d5696-c649-4bba-8a91-94697cd20f08', category_membership_constituted_through_social_performance, conventional).
narrative_ontology:cs_axiom('e94d5696-c649-4bba-8a91-94697cd20f08', foundational, recognition_by_community_members_required_for_authenticity).
narrative_ontology:cs_axiom_status(recognition_by_community_members_required_for_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('e94d5696-c649-4bba-8a91-94697cd20f08', recognition_by_community_members_required_for_authenticity, conventional).
narrative_ontology:cs_reference_frame('e94d5696-c649-4bba-8a91-94697cd20f08', coherent_gendered_social_role_performance).
narrative_ontology:cs_drift_state('e94d5696-c649-4bba-8a91-94697cd20f08', contemporary_institutional_recognition_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e94d5696-c649-4bba-8a91-94697cd20f08', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, gatekeepers_cis_established_performers).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, institutional_role_allocators).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_individuals_requiring_recognition).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, cis_women_policing_boundaries).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANS WOMAN SEEKING RECOGNITION (SNARE) — Identity fused with category membership; cannot exit the performative requirement without abandoning the category itself. Requires continuous proof through dress, demeanor, social positioning. Local gatekeepers (coworkers, family, acquaintances) hold veto power. High extraction: bearing constant surveillance cost, repeated justification burden, risk of sudden invalidation. Suppression is severe: no formal redress for recognition denial, social isolation if boundary violation claimed.
constraint_indexing:constraint_classification(gendered_category_membership__social_role_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CIS WOMAN AS BOUNDARY ENFORCER (TANGLED ROPE) — Experiences the category as coordination mechanism (shared knowledge of social norms, mutual recognition enabling collective action) but is constrained into a policing role by the social role reading itself. Genuine coordination function: shared presentation norms enable mutual recognition and solidarity. But the constraint extracts boundary-maintenance labor from women who must constantly validate/invalidate others' membership. Constrained exit: refusing to police the boundary risks accusations of betraying women's interests or naive inclusivity. Moderate extraction reflecting dual burden.
constraint_indexing:constraint_classification(gendered_category_membership__social_role_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL ROLE ALLOCATOR (ROPE) — HR systems, educational institutions, legal systems treating social role performance as category criterion. Experiences pure coordination: 'woman' as a functional role with associated protections and spaces. The social role reading enables institutional clarity — allocators can use observable performance criteria without parsing internal identity or biological status. Net beneficiary: maintains institutional efficiency and can defer responsibility ('we use standard social role criteria') while extracting conformity demands from applicants.
constraint_indexing:constraint_classification(gendered_category_membership__social_role_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANS RIGHTS COALITION (TANGLED ROPE) — Organized agents (trans organizations, queer theory, legal advocates) see the social role reading as coordinating legitimate recognition but extracting unjust proof burdens. Real coordination function: enables cross-community solidarity, shared institutional strategies. But the constraint enforces performance asymmetrically — trans individuals must exceed cis performance standards to achieve equivalent recognition. Mobile exit: coalition can advocate for alternative readings (identity-based, biological-status mixed) and build alternative spaces (online communities, trans-affirming institutions). Moderate effective extraction because organized agents can shift ground.
constraint_indexing:constraint_classification(gendered_category_membership__social_role_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGAL/ADMINISTRATIVE CLASSIFICATION SYSTEM (PITON) — Social role performance as category criterion persists through institutional inertia in bureaucratic systems despite increasing recognition that it is contested and subject to multiple readings. High theater ratio (0.61): formal procedures (legal document changes, administrative recertification) coexist with informal social verification that remains non-standardized and dependent on local gatekeepers. The constraint is performative — the formal process says one thing (social role criteria are objective, measurable) while the actual gatekeeping happens informally (neighborhood interpretation of femininity, acceptance by peer groups, presentation conformity). Piton classification reflects that the institutional system maintains the criterion despite knowing it is under epistemic challenge.
constraint_indexing:constraint_classification(gendered_category_membership__social_role_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, gender categories necessarily involve social role performance: all category membership is constituted through sustained social interaction and mutual recognition. This is a structural feature of how social ontology works — categories exist because communities recognize and perform them. No performance = no category membership. However, the structural data contradicts the mountain classification. The engine will compute this as a false summit, revealing that the 'categories require performance' framing naturalizes a contingent institutional gatekeeping arrangement as a necessity of social ontology itself.
constraint_indexing:constraint_classification(gendered_category_membership__social_role_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gendered_category_membership__social_role_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gendered_category_membership__social_role_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gendered_category_membership__social_role_reading, TR),
    TR >= 0.70.

:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The social role reading requires proof of performance from trans individuals but stops short of the extreme extraction of a pure snare because institutional recognition pathways exist (legal document changes, HR procedures, institutional acceptance). The extraction is real and sustained — the performance cost is continuous, unequal (trans individuals must exceed cis standards), and subject to local veto — but it is not insurmountable for organized agents. The measurement trajectory (0.28 → 0.35 → 0.38) shows slight accumulation as institutional scrutiny increases, forcing more explicit performance articulation. Suppression (0.52): Moderate-high. Trans individuals and cis women enforcing boundaries both face suppression mechanisms. For trans individuals: no formal appeals process for recognition denial, social isolation risk, precarity of informal acceptance. For cis women: normative expectations that refusal to enforce boundaries is politically suspect. Institutional suppression: legal systems and HR require category determination, creating pressure to maintain gatekeeping even when empirically confused. Theater ratio (0.61): Moderate-high. The constraint shows substantial performative content. Formal institutional procedures (name changes, document updates, HR certification) provide the appearance of objective determination while actual gatekeeping remains informal and distributed. Presentation standards (clothing, demeanor, voice) are treated as objective criteria while remaining culturally contingent and subject to interpretation. The trajectory (0.48 → 0.58 → 0.61) reflects increasing institutionalization of the performance-based criterion, making the theater more visible as institutional systems attempt to codify what remains a social decision.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radically divergent experiences from identical structural position. The trans woman seeking recognition experiences a snare (maximum extraction, identity lock, suppression). The cis woman enforcing boundaries experiences tangled rope (genuine coordination but extracted into policing role). The institutional allocator experiences pure rope (coordination mechanism providing clarity). The trans rights coalition experiences tangled rope with organized mobility (can shift ground or build alternatives). The legal/administrative system experiences piton (performative criteria maintained through inertia). The analytical observer risks mountain classification, naturalizing social performance as an inherent feature of how social categories must work. The engine's false summit detection will flag this as naturalization: the claim that gender categories necessarily require performance is not a law of social ontology but a particular institutional reading of the contested kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural relationship to the social role reading. Trans women seeking recognition are targets: they must provide recognition to gatekeepers (through performance) to receive recognition in return, producing high d. Cis women enforcing boundaries are both partially beneficiaries (maintain category coherence they also inhabit) and partially victims (extracted into labor), producing moderate d. Institutional allocators are beneficiaries with low d: they extract compliance (receive recognition from applicants) and disperse responsibility (defer to criteria). The trans rights coalition, being organized and mobile, has negotiable d: they can refuse the reading's terms and advocate alternatives, producing moderate d. The legal system is a partial beneficiary with low d: it benefits from the clarity the reading provides while being constrained by its own performativity. The analytical observer at civilizational scope faces an oracle gap (Theorem 4): their native instruments cannot detect whether social performance is a natural requirement or an institutional artifact, requiring cross-position analysis to see the construction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the social role reading itself contains the hybrid structure that tangled_rope classification expresses. The constraint genuinely coordinates (enables shared understanding of social role norms, mutual recognition, collective action). It simultaneously extracts (proof burdens from trans individuals, policing burdens from cis women, conformity pressure from everyone). Neither function can be disaggregated from the other — the coordination function depends on enforced boundaries, and the enforcement depends on coordination of shared norms. This is not ambiguity about which type is correct but structural necessity: the social role reading must coordinate through performance standards to function, and coordination through standards necessarily extracts from those who must perform to the standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_sufficiency_threshold,
    'What degree of social role performance suffices for authentic category membership under this reading?',
    'Comparative ethnography: documented cases of acceptance/rejection across different communities; analysis of whether threshold is explicit or implicit; correlation between performance metrics and community acceptance',
    'If threshold is low/permissive: constraint functions as rope (genuine coordination, minimal gatekeeping). If threshold is high/stringent: constraint functions as snare (proof burden becomes extraction). Current empirical answer is ambiguous and community-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_sufficiency_threshold, empirical, 'Performance sufficiency threshold for category membership').

omega_variable(
    gatekeeping_distribution_asymmetry,
    'Is gatekeeping authority distributed equally across community members or concentrated in designated boundary enforcers (e.g., cis women, institutional authorities)?',
    'Social network analysis of recognition decisions; comparison of who performs validation in different contexts (intimate relationships vs workplace vs legal); measurement of veto power concentration',
    'If distributed: constraint is low-suppression rope (many paths to recognition). If concentrated: constraint is high-suppression snare (single authority veto). Current reality is mixed — concentration varies by institutional context.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gatekeeping_distribution_asymmetry, empirical, 'Concentration of gatekeeping authority').

omega_variable(
    reading_kernel_distinction,
    'Is this the social role reading of the kernel, or has the reading itself become the kernel (such that alternative readings are now interpreted as challenges to the kernel rather than sibling readings)?',
    'Historical analysis of policy documents, legal reasoning, institutional criteria; examination of whether institutions acknowledge other readings as legitimate alternatives or position them as incorrect claims about a settled kernel',
    'If social role reading is still a reading: the kernel is genuinely contested and multiple readings coexist. If social role reading has become the kernel: institutional legitimacy is now anchored to this reading, and alternative readings are positioned as denials rather than interpretations. This shifts the entire reading_relations structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether social role reading has become institutionalized as the kernel itself').

omega_variable(
    false_summit_natural_ontology_claim,
    'Is the claim that ''gender categories necessarily require social role performance'' a genuine natural law about social ontology, or a naturalization of a particular institutional arrangement?',
    'Comparative institutional analysis: examination of alternative recognition systems that do not require performance (identity self-declaration, biological-status systems, alternative readings); ethnographic documentation of communities where performance is not gatekeeping criterion',
    'If genuine natural law: the mountain perspective is correct and alternative readings are impossible. If naturalization: the mountain perspective is a false summit, and the engine''s FSM detection is appropriate. Current frame privileges naturalization hypothesis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_ontology_claim, conceptual, 'Whether performance-requirement is natural law or naturalized institutional arrangement').

omega_variable(
    victim_structure_ambiguity,
    'Who is the primary victim of this constraint: trans individuals required to perform, cis women burdened with boundary enforcement, or gender-nonconforming agents excluded by performance criteria?',
    'Qualitative research documenting harm distribution; comparison of extraction burdens (performance cost for trans individuals vs policing cost for cis women vs exclusion cost for nonconforming agents); measurement of differential access to institutional resources by group',
    'If primary victims are trans individuals: snare reading is appropriate. If primary victims are cis women: tangled_rope reading captures extraction-via-boundary-enforcement. If victims are nonconforming agents: constraint may be a snare with different victim set entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_ambiguity, empirical, 'Identity of primary victim group(s)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gendcat_sr_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(gendcat_sr_tr_t10, gendered_category_membership__social_role_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(gendcat_sr_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(gendcat_sr_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gendcat_sr_be_t10, gendered_category_membership__social_role_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(gendcat_sr_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gendcat_sr_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(gendcat_sr_su_t10, gendered_category_membership__social_role_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(gendcat_sr_su_t20, gendered_category_membership__social_role_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% The social role reading is one of three decomposed constraints under the gendered_category_membership kernel. Each reading has distinct ε values reflecting different structural gatekeeping mechanisms. The biological_sex_reading grounds membership in measurable facts (ε ≈ 0.15, mountain). The gender_identity_reading grounds membership in self-identification (ε ≈ 0.22, rope). The social_role_reading grounds membership in performance and recognition (ε ≈ 0.38, tangled_rope). All three affect each other through institutional competition for legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__social_role_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
