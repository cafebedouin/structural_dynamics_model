% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Orthodox Restitution Reading: Hagia Sophia's Ecclesial Legitimacy and Return to Ecclesiastical Control
 *   domain: cultural_heritage/religious_authority/geopolitical_sovereignty
 *
 * SUMMARY:
 *   The Orthodox restitution reading of Hagia Sophia's legitimacy substrate
 *   asserts that the building's foundational purpose as a Christian cathedral
 *   (537 CE–1453 CE) generates permanent ecclesiastical authority that should
 *   either return to Orthodox ecclesiastical control or be honored through
 *   neutral heritage stewardship that acknowledges Byzantine provenance. This
 *   is ONE READING of a contested kernel — the legitimacy substrate that
 *   determines who should control the building and on what grounds. The
 *   restitution claim does not rest on current utility or international law
 *   precedent; it rests on the claim that foundational religious purpose
 *   creates an immutable title that 573 years of interruption (Islamic use
 *   1453–1935, secular museum use 1935–2020, mosque use 2020–present) cannot
 *   erase. This reading instantiates a specific normative commitment:
 *   ecclesiastical legitimacy is primary and durable. The competing sibling
 *   readings (Islamic sovereignty, universal heritage neutrality) rest on
 *   different commitments and produce different constraint structures. This
 *   constraint story models ONLY the Orthodox restitution reading as a clean,
 *   ε-invariant constraint with its own beneficiaries, victims, and authority
 *   structure.
 *
 * KEY AGENTS:
 *   - Eastern Orthodox Diaspora & Ecclesiastical Leadership: Primary beneficiary (organized/mobile) — gains symbolic recognition of foundational identity, historical continuity claim, and potential for liturgical restoration
 *   - Greek State & EU Diplomatic Apparatus: Secondary beneficiary (powerful/arbitrage) — extracts diplomatic leverage in EU-Turkey negotiations, Cyprus disputes, and regional positioning without material enforcement cost
 *   - Turkish State Authority: Primary victim (institutional/constrained) — faces external sovereignty claim, prestige damage, and geopolitical pressure while also benefiting from heritage conservation coordination
 *   - Islamic Worshippers & Turkish Secular Population: Secondary victim (powerless/trapped) — suppressed by the restitution frame's delegitimization of Islamic religious use and secular heritage stewardship
 *   - Global Heritage Governance (UNESCO/ICOMOS): Tertiary actor (institutional/arbitrage) — maintains performative neutrality while practically enforcing status-quo preservation, creating piton effect
 *   - Analytical Observer: Identifies the legitimacy substrate contest as the core structural feature and the committer frame's role in determining whether restitution claim is foundational or constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.28).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.52).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Orthodox Restitution Reading: Hagia Sophia's Ecclesial Legitimacy and Return to Ecclesiastical Control").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/religious_authority/geopolitical_sovereignty").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__orthodox_restitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, 'ae4fce65-a1f0-4055-aa9e-84fa44f07dcd').
narrative_ontology:cs_kernel_codification('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', fixed_text).
narrative_ontology:cs_authority_grounding('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', lineage).
narrative_ontology:cs_interpretation_layer_present('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd').
narrative_ontology:cs_reading_relation('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', foundational, ecclesiastical_legitimacy_primacy).
narrative_ontology:cs_axiom_status(ecclesiastical_legitimacy_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', ecclesiastical_legitimacy_primacy, deontological).
narrative_ontology:cs_axiom('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', secondary, interrupted_worship_incompleteness).
narrative_ontology:cs_axiom_status(interrupted_worship_incompleteness, holdable).
narrative_ontology:cs_axiom_grounding('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', interrupted_worship_incompleteness, theological).
narrative_ontology:cs_reference_frame('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', byzantine_ecclesiastical_primacy).
narrative_ontology:cs_drift_state('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', contemporary_post_2020_mosque_restoration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ae4fce65-a1f0-4055-aa9e-84fa44f07dcd', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state_diplomatic_leverage).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_national_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, secular_heritage_neutrality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISLAMIC WORSHIPPERS & TURKISH SECULARISTS (SNARE) — Trapped by the restitution claim's framing: either the legitimacy of the mosque is denied (religious grounds) or Turkish sovereignty is delegitimized (geopolitical grounds). No exit path that preserves both Islamic worship continuity and national territorial integrity within the restitution frame. Bears suppressive weight of a historical claim that frames their presence as illegitimate occupation.
constraint_indexing:constraint_classification(hagia_sophia_substrate__orthodox_restitution_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TURKISH STATE AUTHORITY (TANGLED ROPE) — Constrained by sovereignty claims and international heritage discourse, but also benefits from the building's status as a museum/heritage site (UNESCO World Heritage, global prestige, international investment in conservation). The constraint combines genuine coordination function (managing a globally significant structure) with asymmetric extraction (external normative claims on national territory). Renovation and conservation are coordination gains; restitution demands are extraction pressure.
constraint_indexing:constraint_classification(hagia_sophia_substrate__orthodox_restitution_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EASTERN ORTHODOX DIASPORA & ECCLESIASTICAL LEADERSHIP (ROPE) — The restitution frame offers coordination benefit: recognition of historical continuity, restoration of ecclesiastical authority, symbolic healing of the 1453 rupture. Exit options are relatively mobile (the claim can be maintained, abandoned, or transformed into other forms of symbolic recognition without enforcement cost). The constraint functions primarily as a coordination mechanism asserting religious and cultural identity continuity.
constraint_indexing:constraint_classification(hagia_sophia_substrate__orthodox_restitution_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: GREEK STATE & EU DIPLOMATIC APPARATUS (SCAFFOLD) — Powerful actors with arbitrage options. The restitution claim yields immediate diplomatic leverage during EU-Turkey negotiations, Cyprus tensions, and regional positioning, but is explicitly NOT pursued as a concrete policy goal (no enforcement mechanism, no material claim on the building itself). This is theatrical leverage: the claim exists to create negotiating room. Sunset logic: the claim's utility expires if Turkey-EU relations normalize, or if it becomes materially enforceable (which would transform it into Snare or Tangled Rope).
constraint_indexing:constraint_classification(hagia_sophia_substrate__orthodox_restitution_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL HERITAGE GOVERNANCE INSTITUTIONS (PITON) — UNESCO, ICOMOS, and international heritage law frame the building as universal humanity patrimony. This framing is largely performative: it asserts neutral universal values while actually serving status-quo preservation (keeping Turkey as custodian, preventing restitution to Greece, preventing religious reclamation by either tradition). The governance ritual persists through institutional inertia — heritage institutions claim neutrality while their practical effect is to freeze the building in its current state. Theater ratio reflects the gap between the universal heritage claim and the actual geopolitical stalemate maintained by institutional consensus.
constraint_indexing:constraint_classification(hagia_sophia_substrate__orthodox_restitution_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes that the restitution claim instantiates a specific reading of the legitimacy substrate — the claim that foundational religious/ecclesiastical purpose generates permanent authority. This reading genuinely coordinates identity continuity (Orthodox ecclesiastical tradition, historical linkage to Byzantium) but asymmetrically extracts by delegitimizing 573 years of Islamic and secular use, denying Turkish sovereignty, and suppressing alternative readings. The constraint is structurally a hybrid coordination-extraction mechanism, not a false natural law.
constraint_indexing:constraint_classification(hagia_sophia_substrate__orthodox_restitution_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hagia_sophia_substrate__orthodox_restitution_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hagia_sophia_substrate__orthodox_restitution_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, TR),
    TR >= 0.70.

:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The restitution claim carries substantial symbolic weight but zero material enforcement pathway. Turkish sovereignty is effectively non-negotiable within current geopolitical constraints; restitution through coercion would require NATO collapse or unthinkable costs. The extractiveness reflects the claim's ability to delegitimize Turkish stewardship and generate diplomatic pressure (delegitimization is a form of extraction from Turkey's authority standing), but the absence of enforcement pathways caps it below snare range. Over the 30-year interval, extractiveness rose from 0.18 to 0.28 as the claim became more rhetorically salient in EU-Turkey discourse, but the trajectory is shallow — the claim's mobilization potential is stable. Suppression (0.52): Moderate-high. The restitution frame suppresses Islamic worship claims and secular heritage framings by asserting that foundational religious purpose overrides post-1453 continuity. However, suppression is not total — the building currently functions as a mosque (post-2020), Turkish sovereignty is maintained, and heritage preservation continues. The suppression operates at the legitimacy substrate level (what grounds rightful authority) rather than at the enforcement level (who physically controls the building). Theater ratio (0.68): Moderate-high. Increasing over time. The global heritage governance narrative frames Hagia Sophia as universal patrimony, UNESCO World Heritage Site, and exemplary Ottoman-Byzantine synthesis — all framing that performs neutrality while actually maintaining status quo (Turkish control + international oversight, preventing both restitution and full Islamic reclamation). The restitution claim itself is invoked primarily in diplomatic contexts with no material implementation pressure, making it partially performative leverage rather than a concrete policy goal. The theater ratio's rise (0.55 → 0.72) reflects the increasing performativity of the restitution claim in EU-Turkey negotiations as actual implementation remains implausible.
 *
 * PERSPECTIVAL GAP:
 *   The restitution reading produces multiple incompatible classifications from different structural positions. Turkish state authority and Islamic worshippers experience the claim as extractive and suppressive (snare from the trapped perspective, tangled rope from the constrained institutional perspective). Orthodox ecclesiastical tradition experiences it as a coordination mechanism restoring identity continuity (rope). Greek diplomatic apparatus and EU institutions experience it as useful leverage with no enforcement cost (scaffold / piton — temporary utility maintained through unresolved status). The analytical observer recognizes all six types as valid perspectives on the same constraint, but sees the core structure as tangled rope: genuine coordination function (identity continuity) combined with asymmetric extraction (delegitimization of Turkish/Islamic claims). The prospectival gap reveals that the dispute is fundamentally about which legitimacy reading governs — no single classification is 'correct' because the answerer's legitimacy commitment is constitutive of how they perceive the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The restitution claim's directionality differs radically by structural position. Orthodox beneficiaries perceive low or negative extraction (d ≈ 0.15–0.25) — the claim restores what they perceive as rightfully theirs, requiring no cost to them. Turkish state, positioned as the target of the sovereignty claim, perceives high extraction (d ≈ 0.70–0.80) — the claim delegates Turkish authority as illegitimate occupation. Greek diplomatic apparatus, with arbitrage options, perceives the claim as leverage with manageable cost (d ≈ 0.40–0.50) — they can invoke or abandon it depending on negotiating context. Islamic worshippers, trapped by the legitimacy frame's delegitimization of their worship, perceive maximum extraction (d ≈ 0.85–0.95) — the frame suppresses their presence itself. The analytical observer, measuring the structural relationships, recognizes the committer frame as constitutive: the restitution reading DEFINES who extracts from whom by deciding which legitimacy principle governs. This is not a natural directionality — it is a committer choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The restitution reading avoids mandatrophy (mislabeling coordination as extraction or vice versa) by acknowledging both functions within a tangled rope classification. The reading genuinely coordinates identity continuity and ecclesiastical restoration — this is not extractive leverage in disguise. BUT the reading also genuinely asymmetrically extracts by delegitimizing Turkish sovereignty and suppressing alternative legitimacy substrates (Islamic continuity, secular heritage, international neutrality). The tangled rope classification holds both simultaneously: the constraint is a coordination mechanism for the Orthodox tradition AND an extraction mechanism for the Turkish state/Islamic worshippers/secular governance. Avoidance of the false-natural-law (mountain) trap occurs because the analytical observer recognizes the restitution reading as one committed position, not as objective historical fact. The building's 'true' legitimacy is not given by physics or mathematics — it is constituted by which normative commitment an authority accepts. The restitution reading asserts one such commitment (foundational purpose primacy); the sibling readings assert others. The engine's mandatrophy resolution protocol confirms that tangled rope is the appropriate classification because the constraint exhibits genuine coordination function (restoring identity continuity) and substantial extraction (delegitimizing alternative authorities) simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_substrate_committer_frame,
    'Which reading of the legitimacy substrate governs Hagia Sophia''s authority: foundational religious purpose (Orthodox restitution), post-1453 continuous use (Islamic/secular), universal heritage value (international neutrality), or Turkish national sovereignty?',
    'Explicit recognition that the kernel admits multiple readings; no empirical resolution possible. This is a committed position omega — the answer depends on which legitimacy frame the adjudicating authority accepts.',
    'If foundational religious purpose governs: Orthodox restitution claim is structurally sound, Turkish sovereignty is secondary, international heritage law is performance masking geopolitical stalemate (piton reading confirmed). If post-1453 use governs: restitution claim is historical revision, current management is legitimate continuation, Orthodox reading naturalizes a false legitimacy claim. If universal heritage governs: all particularist claims (Orthodox, Turkish, Islamic) are suppressed equally; neutrality is the content.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_substrate_committer_frame, conceptual, 'Which legitimacy substrate reading governs Hagia Sophia''s rightful authority').

omega_variable(
    enforcement_pathway_absence,
    'What would material enforcement of the Orthodox restitution claim require, and why is it practically infeasible?',
    'Structural analysis: restitution would require either (a) Turkish government voluntary cession (zero probability without catastrophic geopolitical reordering), (b) EU/NATO coercion (incompatible with Turkey''s NATO membership and would trigger regional destabilization exceeding any religious restoration benefit), (c) international tribunal enforcement (no mechanism exists; Hague court has no jurisdiction over national territory without consent). Absence of enforcement pathway keeps the claim in scaffold/piton range rather than snare/violent conflict.',
    'If enforcement became possible: extractiveness would rise dramatically (0.28 → 0.65+), suppression on Turkish agents would become severe (0.52 → 0.85+), classification would shift toward snare. Absence of pathway keeps the constraint in theatre/diplomatic leverage range. Low extractiveness reflects practical unenforcability, not structural weakness of the claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_pathway_absence, empirical, 'Feasibility of material enforcement for Orthodox restitution').

omega_variable(
    sibling_reading_incompatibility_structure,
    'Are the Orthodox restitution, Islamic sovereignty, and universal heritage readings truly incompatible (forecloses logic) or merely competing within the same dispute (coexists_with logic)?',
    'Logical analysis of foundational axioms: Orthodox restitution requires that foundational religious purpose generates permanent authority (axiom: ecclesiastical_legitimacy_primacy). Islamic sovereignty reading requires that continuous post-1453 use and Turkish state control generate legitimate authority (axiom: possession_and_state_legitimacy). Universal heritage reading requires that all particularist claims be suppressed in favor of humanity-wide patrimony (axiom: universal_heritage_primacy). Do these axioms logically foreclose each other, or do they simply occupy different parties'' commitments? Answer: they coexist across parties but foreclose within any single authority framework — a Turkish state cannot simultaneously assert sovereignty and concede ecclesiastical authority; the Orthodox tradition cannot simultaneously claim foundational legitimacy and accept that 573 years of interrupted worship nullify the claim.',
    'If coexists_with dominates: the constraint is a permanent geopolitical fixture, managed through diplomatic stalemate (piton/scaffold equilibrium). If forecloses logic applies: one reading will eventually dominate, requiring material resolution or formal repudiation of sibling readings by the relevant authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_incompatibility_structure, conceptual, 'Whether sibling readings foreclose each other or coexist in perpetual dispute').

omega_variable(
    symbolic_generativity_vs_material_extraction,
    'Is the restitution claim''s primary function to generate symbolic identity continuity (coordination reading) or to extract diplomatic leverage and delegitimize Turkish sovereignty (extraction reading)?',
    'Discourse analysis: track when the claim is invoked (Greece-Turkey negotiations, EU-Turkey relations, Orthodox ecclesiastical contexts), who invokes it (Greek diplomats, Orthodox clergy, international heritage advocates), and what outcomes follow (diplomatic pressure, religious ritual claims, heritage preservation leverage vs. actual restitution proposals). If primarily invoked for diplomatic leverage with no material implementation push, extractiveness is lower (coordination + leverage = tangled rope). If invoked to delegitimize Turkish presence as such, extractiveness is higher (pure extraction = snare).',
    'If coordination-dominant: the constraint functions as identity continuity mechanism (ε ≈ 0.25–0.35, rope/scaffold range). If extraction-dominant: it functions as delegitimization mechanism (ε ≈ 0.50–0.70, snare/tangled rope). Current assessment (ε = 0.28, tangled rope) assumes mixed function with coordination slightly dominant but extraction substantial enough to suppress alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_generativity_vs_material_extraction, empirical, 'Whether restitution claim is coordination mechanism or extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_orth_theater_t0, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(hagia_orth_theater_t15, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 15, 0.68).
narrative_ontology:measurement(hagia_orth_theater_t30, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(hagia_orth_extract_t0, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(hagia_orth_extract_t15, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(hagia_orth_extract_t30, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__orthodox_restitution_reading, 0.12).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, istanbul_church_restoration_program).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, turkey_eu_religious_freedom_dispute).

% DUAL FORMULATION NOTE:
% The restitution reading is one of three structurally distinct constraints sharing the contested kernel (hagia_sophia_substrate). Each reading has its own extractiveness value, authority grounding, and legitimacy substrate. The three readings coexist in perpetual geopolitical dispute; none forecloses the others because they rest on different normative commitments (ecclesiastical vs. secular/national vs. universal). All three are linked via network.affects_constraints to show the family relationship and the incompatibility structure. Do NOT merge them into a single 'Hagia Sophia legitimacy' constraint — the ε-invariance principle requires separate stories when different measurement framings produce different extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__orthodox_restitution_reading, institutional, 0.75).
constraint_indexing:directionality_override(hagia_sophia_substrate__orthodox_restitution_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
