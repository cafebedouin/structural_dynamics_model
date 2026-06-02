% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Gelassenheit Separation via Artifact Prohibition (Artifact Reading)
 *   domain: religious_studies/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   The Gelashenheit separation principle ('surrender to divine will,'
 *   'rejection of worldly attachment') admits multiple interpretations of
 *   what separation means and how it is enforced. This constraint
 *   instantiates the ARTIFACT READING: separation is enforced through
 *   prohibition of technological artifacts that *resemble* worldly goods,
 *   regardless of actual function or off-grid use. A solar panel is forbidden
 *   not because it draws power from worldly systems but because it looks like
 *   a worldly artifact; modern synthetic fabrics are prohibited even when
 *   sourced locally because they resemble commercial products. This reading
 *   prioritizes visible material markers over theological principle or
 *   practical consequence. It creates a high-extraction constraint because
 *   the artifact prohibition is mechanistically enforced (elders determine
 *   what 'resembles worldly goods'), generates visibility compliance
 *   (community members are easily monitored), and suppresses material
 *   innovation capacity while maintaining group boundary distinction. The
 *   constraint has strengthened over the measurement interval (ε rising from
 *   0.55 to 0.68, theater_ratio rising from 0.48 to 0.65) as technological
 *   options in the external world have proliferated and the need to enforce
 *   visible distinction has intensified. The suppression requirement has also
 *   risen (0.75 to 0.82) as younger community members face greater external
 *   technological exposure and must be more actively constrained to maintain
 *   separation visibility.
 *
 * KEY AGENTS:
 *   - Younger/Constrained Household Members: Primary victims (powerless/trapped) — raised within the community, identity constituted through the tradition, face maximum suppression and no exit mechanism
 *   - Identity-Fused Adult Community Members: Secondary victims (moderate/identity_locked) — structurally mobile but identity bound to the tradition; exit would require abandoning self-concept
 *   - Community Elders/Leadership: Primary beneficiaries (organized/constrained) — consolidate hierarchical authority through interpretation of 'worldly artifacts,' benefit from constraint enforcement while experiencing constrained exit (losing authority if they defect)
 *   - Ecclesiastical Authority Structure: Institutional beneficiary (institutional/arbitrage) — derives benefit from the constraint (theological clarity, enforcement simplicity, institutional coherence); has exit options (can reinterpret doctrine)
 *   - Material Innovation Capacity: Structural victim (powerless/trapped) — abstract capacity suppressed by the prohibition; cannot develop alternative technologies even for community benefit
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks false summit classification by naturalizing what is an institutional choice among competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.68).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.82).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, snare).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Gelassenheit Separation via Artifact Prohibition (Artifact Reading)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious_studies/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, 'd6285134-f514-45ba-ae64-8fef54a583f9').
narrative_ontology:cs_kernel_codification('d6285134-f514-45ba-ae64-8fef54a583f9', fixed_text).
narrative_ontology:cs_authority_grounding('d6285134-f514-45ba-ae64-8fef54a583f9', lineage).
narrative_ontology:cs_interpretation_layer_present('d6285134-f514-45ba-ae64-8fef54a583f9').
narrative_ontology:cs_reading_relation('d6285134-f514-45ba-ae64-8fef54a583f9', gelashenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6285134-f514-45ba-ae64-8fef54a583f9', gelashenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('d6285134-f514-45ba-ae64-8fef54a583f9', foundational, separation_enforced_via_artifact_visibility).
narrative_ontology:cs_axiom_status(separation_enforced_via_artifact_visibility, holdable).
narrative_ontology:cs_axiom_grounding('d6285134-f514-45ba-ae64-8fef54a583f9', separation_enforced_via_artifact_visibility, conventional).
narrative_ontology:cs_axiom('d6285134-f514-45ba-ae64-8fef54a583f9', foundational, interpretation_authority_vested_in_elders).
narrative_ontology:cs_axiom_status(interpretation_authority_vested_in_elders, holdable).
narrative_ontology:cs_axiom_grounding('d6285134-f514-45ba-ae64-8fef54a583f9', interpretation_authority_vested_in_elders, conventional).
narrative_ontology:cs_reference_frame('d6285134-f514-45ba-ae64-8fef54a583f9', original_separation_principle).
narrative_ontology:cs_drift_state('d6285134-f514-45ba-ae64-8fef54a583f9', contemporary_artifact_enforcement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6285134-f514-45ba-ae64-8fef54a583f9', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, community_elders).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, tradition_maintainers).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, younger_community_members).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, material_innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED HOUSEHOLD MEMBER (SNARE) — Raised within the community; socialization and identity entirely constituted through the tradition. Faces maximum suppression: visible distinction enforcement, limited material access, social exclusion for innovation, family/kinship consequences for defection. No escape mechanism — exit means complete severance from family, faith identity, economic support, and social standing. The prohibition's framing as 'spiritual separation' masks the material extraction: denial of efficient labor-saving technology (solar panels forbidden as 'worldly' despite off-grid function), restricted fabric choices regardless of utility, exclusion from modern knowledge and skill networks.
constraint_indexing:constraint_classification(gelassenheit_separation__artifact_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: IDENTITY-FUSED MEMBER (SNARE via identity_locked) — Structurally mobile (has income, housing options, legal options); but identity constituted through the tradition itself. Self-concept is fused with 'the faithful one,' 'the obedient child,' 'the keeper of the way.' Exit would require abandoning not just a rule set but the entire identity frame. Perceives the constraint as immutable at the biographical horizon because their identity cannot imagine stepping outside it. Materialized suppression: when identity-locked agents attempt partial defection (wearing hidden modern clothing, using hidden technology), they experience crushing internal conflict and community surveillance that reinforces the lock.
constraint_indexing:constraint_classification(gelassenheit_separation__artifact_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: COMMUNITY LEADERSHIP (TANGLED ROPE) — Faces genuine coordination problem: maintaining community cohesion, transmitting tradition, reinforcing group identity boundaries against external cultural pressure. The constraint DOES solve this coordination function — visible distinction markers, technology prohibitions, material boundaries create strong in-group/out-group distinction necessary for long-term community maintenance. BUT: leadership also extracts from the constraint. Controlling the interpretation of 'worldly artifacts' and 'spiritual separation' consolidates hierarchical authority: elders decide what is permitted, younger members must defer, legitimacy flows upward. Leadership experiences the constraint as both necessary coordination and beneficial extraction. Exit options are constrained (leaving means losing authority position and community standing), but the constraint also benefits them materially and institutionally.
constraint_indexing:constraint_classification(gelassenheit_separation__artifact_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: ECCLESIASTICAL AUTHORITY (ROPE) — Experiences the artifact reading as coordination solution: clear doctrine, enforceable boundaries, visible compliance markers, intergenerational transmission mechanism. The reading simplifies the theological problem ('What does separation mean?') into observable rules (no solar panels, restricted fabrics, visible markers). Ecclesiastical authority has arbitrage options — can adjust doctrine, reinterpret tradition, or shift enforcement. Derives benefit from the constraint (theological clarity, institutional coherence, enforcement simplicity) with modest enforcement cost. No predatory extraction — the authority structure is aligned with the coordination function it serves.
constraint_indexing:constraint_classification(gelassenheit_separation__artifact_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: HISTORICAL DOCTRINE AS PITON (CIVILIZATIONAL) — From a long-horizon view, the artifact reading is a degraded instantiation of an original theological commitment. The original principle (mystical separation from worldly attachment) has become increasingly literalized and formalized into object-based rules. Theater ratio is high: prohibiting solar panels because they 'resemble worldly artifacts' is largely performative — the rule maintains a boundary marker more than a spiritual principle. The doctrine persists through institutional inertia and ritualization, not because the artifact reading is the strongest theological interpretation. Alternative readings (principle-based, consequence-based) might better capture the original intent, but institutional momentum preserves the artifact reading.
constraint_indexing:constraint_classification(gelassenheit_separation__artifact_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational perspective, visible distinction through material artifact prohibition might appear immutable: group identity requires boundary markers; technology choices necessarily signal cultural identity; the tension between tradition-maintenance and material efficiency is a structural feature of all closed communities. However, this reading is a FALSE SUMMIT. The constraint benefits specific institutional actors (elders, ecclesiastical authority) and is actively enforced through suppression mechanisms (social exclusion, identity lock, family consequences). The 'natural law' framing obscures that the artifact reading is a *choice among interpretations* — the sibling principle and consequence readings would produce different constraints with different suppression profiles.
constraint_indexing:constraint_classification(gelassenheit_separation__artifact_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gelassenheit_separation__artifact_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gelassenheit_separation__artifact_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, TR),
    TR >= 0.70.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The artifact reading creates significant extraction from constrained community members in multiple forms: (1) denial of material labor-saving technologies (solar panels, modern fabrics) that would benefit the household despite being off-grid or locally sourced; (2) cognitive overhead of monitoring artifact resemblance rather than function; (3) knowledge suppression — community members cannot develop or access technologies that look modern even if they serve community values; (4) generational burden — younger members bear higher suppression cost than elders who benefited from less strict enforcement in their youth. The extractiveness is not maximal (not 0.85+) because the ecclesiastical authority does provide genuine coordination benefit (group identity, tradition transmission, boundary clarity) alongside the extraction. Suppression (0.82): Very high. Enforcement mechanisms include: (1) social surveillance — visible artifacts are easily detected and sanctioned; (2) family consequences — defection from the artifact rule triggers kinship exclusion; (3) identity suppression — the constraint is bound to religious identity, making questioning it equivalent to questioning faith; (4) knowledge sequestration — younger members are systematically prevented from learning about modern technologies even in neutral contexts; (5) alternative foreclosure — few visible off-ramps from the constraint without community rupture. Theater ratio (0.65): Moderate-high. The prohibition operates largely through performative enforcement: elders decide what 'resembles worldly goods' through subjective judgment rather than objective criteria; the enforcement ritual (admonishment, family discussion, community observation) is often more important than any practical effect of the prohibition. Modern fabrics prohibited despite identical function to traditional materials signals this is boundary maintenance theater rather than functional necessity. However, the theater is not total (0.50+) because actual material innovation is genuinely suppressed and members do internalize the rules as meaningful spiritual markers, not merely understood performances.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a maximal perspectival gap between the trapped/identity_locked community members (who see a snare with no escape) and the ecclesiastical authority (who see a rope enabling group coordination). Younger members experience suppression that increases over time as external technology proliferates; leadership experiences the constraint as increasingly valuable (theater_ratio rises) for maintaining boundaries against external pressure. The piton perspective reveals that the artifact reading may be a historically contingent choice: the original Gelassenheit principle could be interpreted as function-based (reject worldly material accumulation) or consequence-based (permit technologies that serve community values without worldly dependence), but the artifact reading (reject visible markers of worldliness) is easier to enforce institutionally and thus has become the default. This suggests institutional momentum rather than theological superiority — the reading is maintained because it works as a control mechanism, not because it best expresses the original principle.
 *
 * DIRECTIONALITY LOGIC:
 *   The artifact reading concentrates power upward (toward elders and ecclesiastical authority) by vesting interpretation of 'worldly artifact' in a small set of decision-makers. Constrained household members and identity-locked members experience high d (0.85+ for trapped members, ~0.80 for identity_locked) because they bear the extraction costs with minimal exit options. Leadership experiences moderate d (~0.45) because they experience both coordination benefit (group cohesion) and extraction benefit (consolidated authority), with constrained exit (leaving means authority loss). Ecclesiastical authority experiences low d (~0.20) because the constraint is largely beneficial to them with low cost and high arbitrage. The analytical observer at the civilizational level experiences d~0.72 (attempting to see the structure objectively) but is at risk of naturalizing the institutional choice as immutable law. The directionality derivation confirms snare classification for the powerless perspectives and tangled_rope for the organized perspective (mixed coordination and extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve mandatrophy through perspectival plurality. The snare and tangled_rope classifications are not relativistic readings of the same phenomenon — they reflect a genuine structural asymmetry. The powerless members experience extraction without compensation; the leadership experiences coordination benefit alongside extraction. The constraint is NOT a natural law (the mountain perspective is a false summit). The mandatrophy is resolved by recognizing that the artifact reading is ONE CHOICE AMONG COMPETING INTERPRETATIONS of the Gelashenheit principle. The sibling readings (principle-based, consequence-based) would produce different suppression profiles and different χ values. The artifact reading is not 'the' answer to what separation means — it is the answer that an institutional authority chose because it was most enforceable. This recognition dissolves the apparent contradiction between the snare and rope classifications: they are both accurate descriptions of different agents' structural positions within a constraint whose existence is contingent on this particular reading being selected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artifact_vs_function_boundary,
    'Is the prohibition grounded in artifact appearance/resemblance or in functional effect? Does a solar panel become forbidden because it looks ''worldly'' or because it represents material autonomy from the community?',
    'Compare actual enforcement patterns: are functionally identical technologies treated differently based on appearance? (E.g., hand-cranked and solar generators producing the same electrical output — is one permitted and one forbidden?) Analyze elder explanations and community debates about specific cases.',
    'If grounded in appearance: artifact reading is confirmed and ε remains high (~0.68). If grounded in function: constraint decomposes into separate story about material autonomy/autonomy_suppression with different ε. If mixed: constraint needs decomposition into artifact_appearance and functional_autonomy stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(artifact_vs_function_boundary, empirical, 'Whether prohibition is artifact-based or function-based').

omega_variable(
    reading_selection_driver,
    'Is the artifact reading chosen because it best matches the original theological principle, or because it is the most enforceable interpretation? Are there documented theological disputes where the artifact reading was selected *despite* weaker theological grounding?',
    'Historical analysis of theological texts, schismatic disputes, and doctrine evolution. Interviews with religious scholars about the theological strength of artifact vs. principle vs. consequence readings. Analysis of enforcement mechanisms: do elders justify the prohibition primarily in theological terms or in boundary-maintenance terms?',
    'If artifact reading is selected for theological strength: validates the ecclesiastical authority perspective. If selected for enforceability: supports the snare classification and piton perspective — the reading is maintained through institutional momentum rather than theological merit. Affects omega analysis of cs_structure.authority_grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_driver, conceptual, 'Why the artifact reading is selected over sibling readings').

omega_variable(
    identity_lock_durability,
    'For identity_locked members, how durable is the lock if external conditions change? Do exposed community members show higher exit rates? Does education or access to outside reference groups erode the identity lock or merely create internal conflict?',
    'Longitudinal tracking of members exposed to external environments (higher education, employment outside community, internet access). Analysis of defection trajectories: do members with partial exposure exit, remain with internal conflict, or become advocates for constraint modification?',
    'If lock is highly durable: identity_locked exit option is confirmed and snare classification is firm. If lock erodes with exposure: exit_options for younger members should shift toward constrained/mobile, and perspective 2 classification should shift toward tangled_rope or even rope. Affects the mechanism of suppression: if structural barriers are doing most suppressive work, ε may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Durability of identity-lock suppression mechanism').

omega_variable(
    sibling_reading_mutual_foreclosure,
    'Do the artifact, principle, and consequence readings mutually foreclose each other, or can they coexist within the same doctrinal framework? Is there theological incommensurability or merely institutional preference?',
    'Analysis of theological texts and ecclesiastical authorities: can a community committed to the principle reading also acknowledge the artifact reading as valid? Can the consequence reading be layered under either artifact or principle? Do documented schisms derive from incompatible readings or from power struggles between institutional factions?',
    'If readings foreclose each other: reading_relations should be coexists_with or influences (not forecloses, which is rare). If artifact reading explicitly forecloses the others: the artifact reading must be presented as the only valid reading and alternative readings must be wrong. Affects cs_structure.axioms grounding_type and reference_frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_mutual_foreclosure, conceptual, 'Whether sibling readings are logically compatible or mutually exclusive').

omega_variable(
    false_summit_mechanism,
    'Is this constraint presented as a natural law of group identity maintenance, and does that framing obscure the role of ecclesiastical authority in *choosing* this reading over the sibling readings? Is naturalization the cover story?',
    'Ethnographic analysis of how the artifact reading is justified: are members told ''this is what separation requires'' (naturalization) or ''this is what the authorities have decided separation requires'' (institutional choice)? Do teaching materials present this reading as deduced from principle or as handed-down tradition?',
    'If naturalized: false summit signature should trigger and engine should flag this as an engineered natural-law claim. If framed as institutional choice: the mountain classification at perspective 6 is incorrect and the constraint should classify as tangled_rope or snare across all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mechanism, conceptual, 'Whether the constraint naturalizes a contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gelass_art_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(gelass_art_tr_t10, gelassenheit_separation__artifact_reading, theater_ratio, 10, 0.57).
narrative_ontology:measurement(gelass_art_tr_t20, gelassenheit_separation__artifact_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(gelass_art_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gelass_art_be_t10, gelassenheit_separation__artifact_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(gelass_art_be_t20, gelassenheit_separation__artifact_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gelass_art_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(gelass_art_su_t10, gelassenheit_separation__artifact_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement(gelass_art_su_t20, gelassenheit_separation__artifact_reading, suppression_requirement, 20, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelashenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% The Gelashenheit separation kernel decomposes into three constraint stories per the ε-invariance principle. The artifact reading (this file) produces high epsilon and high suppression because it vests interpretation in a small authority set and creates identity-lock suppression. The principle reading would decompose the constraint into function-based (lower epsilon) and authority-based (higher epsilon) components. The consequence reading would produce higher theater_ratio and lower enforcement-efficiency suppression. All three stories share the same kernel (Gelashenheit principle) but differ in how the principle is instantiated into observable rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
