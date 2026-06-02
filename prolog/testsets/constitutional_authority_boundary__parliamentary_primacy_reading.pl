% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Constitutional Authority Boundary: Parliamentary Primacy Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents one reading of the contested kernel
 *   'constitutional authority boundary' — specifically, the doctrine that
 *   elected legislatures retain ultimate interpretive authority over
 *   constitutional meaning and can revise constitutional protections through
 *   ordinary or entrenched legislation without judicial veto. This reading
 *   instantiates parliamentary supremacy (also called parliamentary
 *   sovereignty), the dominant constitutional tradition in Westminster
 *   systems (UK, Australia, Canada pre-1982) and elements of many continental
 *   European democracies. The constraint exhibits the characteristic
 *   structure of a tangled_rope: it serves a genuine coordination function
 *   (enabling democratic majorities to implement electoral mandates without
 *   judicial obstruction) while simultaneously extracting protection against
 *   majoritarian rights revision from supramajority minorities and permanent
 *   minorities. The constraint's extractiveness (0.18) reflects moderate
 *   asymmetry — not the high extraction of a pure snare, because legislatures
 *   do negotiate minority rights as political necessity even under supremacy
 *   doctrine, and because the constraint's legitimacy rests on democratic
 *   principles that include deliberation and coalition-building. The
 *   theater_ratio (0.42) reflects that formal parliamentary debate,
 *   legislative amendment procedures, and the architectural appearance of
 *   separation of powers all perform legitimating functions while the core
 *   power dynamic (legislature can revise any protection) remains unchanged.
 *
 * KEY AGENTS:
 *   - Elected Legislature: Primary beneficiary (institutional/arbitrage) — retains authority to define constitutional meaning; can implement mandates without judicial veto
 *   - Governing Coalition / Electoral Majority: Secondary beneficiary (powerful/mobile) — benefits from democratic legitimacy and direct policy implementation
 *   - Supramajority Minorities: Primary victim (moderate/constrained) — constrained by the supremacy doctrine; cannot appeal to entrenched judicial review; benefit from legislative deliberation but face majoritarian revision risk
 *   - Permanent Minorities (Structurally Excluded Groups): Primary victim (powerless/trapped) — face maximum extraction; no legislative coalition-building power; rights protection depends entirely on contingent parliamentary goodwill
 *   - The Judiciary: Institutional degradation (institutional/constrained) — role reduced to interpretation and advisory function under supremacy doctrine; institutional theater maintained but substantive authority hollowed
 *   - Constitutional Reform Movements: Organized challengers (organized/mobile) — see supremacy as revisable through democratic amendment; represent scaffold perspective with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.18).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.35).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Constitutional Authority Boundary: Parliamentary Primacy Reading").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, 'e737510d-b2e1-44d5-a22a-252d84fa0f9f').
narrative_ontology:cs_kernel_codification('e737510d-b2e1-44d5-a22a-252d84fa0f9f', formalized).
narrative_ontology:cs_authority_grounding('e737510d-b2e1-44d5-a22a-252d84fa0f9f', lineage).
narrative_ontology:cs_interpretation_layer_present('e737510d-b2e1-44d5-a22a-252d84fa0f9f').
narrative_ontology:cs_reading_relation('e737510d-b2e1-44d5-a22a-252d84fa0f9f', judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e737510d-b2e1-44d5-a22a-252d84fa0f9f', coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('e737510d-b2e1-44d5-a22a-252d84fa0f9f', foundational, legislature_is_primary_interpreter).
narrative_ontology:cs_axiom_status(legislature_is_primary_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('e737510d-b2e1-44d5-a22a-252d84fa0f9f', legislature_is_primary_interpreter, deontological).
narrative_ontology:cs_axiom('e737510d-b2e1-44d5-a22a-252d84fa0f9f', foundational, judicial_review_subordinate_to_legislature).
narrative_ontology:cs_axiom_status(judicial_review_subordinate_to_legislature, holdable).
narrative_ontology:cs_axiom_grounding('e737510d-b2e1-44d5-a22a-252d84fa0f9f', judicial_review_subordinate_to_legislature, deontological).
narrative_ontology:cs_reference_frame('e737510d-b2e1-44d5-a22a-252d84fa0f9f', parliamentary_legislative_supremacy).
narrative_ontology:cs_drift_state('e737510d-b2e1-44d5-a22a-252d84fa0f9f', contemporary_post_charter_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e737510d-b2e1-44d5-a22a-252d84fa0f9f', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, governing_coalition).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_independence).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, individual_rights_against_majoritarian_revision).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELECTED LEGISLATURE (ROPE) — Experiences the constraint as pure coordination: parliamentary supremacy enables democratic majorities to implement mandates without judicial veto. The constraint solves the legitimate collective action problem of translating electoral will into binding law. Net beneficiary with arbitrage options (can redefine constitutional meaning through legislation). Experiences minimal extraction — the mechanism serves their substantive goals.
constraint_indexing:constraint_classification(constitutional_authority_boundary__parliamentary_primacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL RIGHTS HOLDERS / SUPRAMAJORITY MINORITIES (TANGLED ROPE) — Face mixed coordination and extraction. Constrained by the supremacy doctrine (cannot appeal to entrenched judicial review to protect minority rights). Yet also benefit from the legislative deliberation process, negotiation, and the potential for coalition-building. The constraint coordinates democratic legitimacy while extracting protection against majoritarian revision. High suppression (cannot exit to alternative rights-protection framework without constitutional amendment). Some extraction from the supramajority gate (legislative majorities can redefine protections), but coordination function exists (legislatures do negotiate minority rights as political necessity, even under supremacy doctrine).
constraint_indexing:constraint_classification(constitutional_authority_boundary__parliamentary_primacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PERMANENT MINORITIES / NON-NEGOTIATED GROUPS (SNARE) — Groups structurally excluded from legislative coalition-building (geographically dispersed, politically weak, culturally stigmatized) experience pure extraction under parliamentary supremacy. They face the full force of majoritarian revision with no exit mechanism — trapped in a system where rights protection depends on legislative goodwill rather than entrenched law. Theater ratio is moderate (legislative deliberation has some real protective function), but extraction is severe because the suppression gate is high: no judicial override, no constitutional entrenchment, no protection mechanism that does not depend on contingent parliamentary favor.
constraint_indexing:constraint_classification(constitutional_authority_boundary__parliamentary_primacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: THE JUDICIARY / JUDICIAL INSTITUTION (PITON) — The judicial role under parliamentary supremacy is degraded from its pre-supremacy form. Courts retain ceremonial and advisory functions (interpreting parliamentary intent, enforcing lower-order rules) but lack substantive authority over constitutional meaning. The constraint maintains this institutional inertia: courts continue to exist and pronounce on law, creating theater of independent judicial review, but with minimal functional power. The institution persists because legislatures tolerate it and because incomplete supremacy (requiring legislative reversal of judicial decisions) maintains the appearance of institutional separation. Theater ratio is moderate-high (judicial opinions create legitimacy theater around legislative will) and extractiveness is low (the judiciary is constrained, not enriched, under this reading). Piton classification derives from the theatrical maintenance of an institution whose substantive function has been hollowed.
constraint_indexing:constraint_classification(constitutional_authority_boundary__parliamentary_primacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / DEMOCRATIC INEVITABILITY VIEW (MOUNTAIN) — From a civilizational perspective, some form of ultimate authority must reside somewhere in any legal system; the parliament is the most democratic locus, making parliamentary supremacy the natural or inevitable institutional solution. This perspective naturalizes the reading as an immutable feature of democratic legitimacy. However, the analytical observer is vulnerable to false-summit detection: the parliamentary primacy doctrine is a contingent institutional choice, not a law of nature. Sibling readings (judicial supremacy, coordinate construction) are equally available within democratic theory and practice.
constraint_indexing:constraint_classification(constitutional_authority_boundary__parliamentary_primacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CONSTITUTIONAL REFORM MOVEMENTS (SCAFFOLD) — Organized civil-society actors (human rights organizations, indigenous movements, religious minorities) see parliamentary supremacy as a temporary institutional arrangement available for democratic revision. This perspective treats the constraint as a coordination problem with a built-in sunset: through constitutional amendment (supermajority gate) or constitutional rewriting, actors can shift to an alternative authority structure (entrenched rights, judicial review, coordinate construction). The scaffold derives from the recognition that supremacy doctrine is itself a constitutional choice, not a natural law, and therefore available for democratic reconstruction. Theater ratio is low (the actual challenge to supremacy is structural, not performative) and extraction is moderate (the constraint inhibits reform by requiring supermajority consensus, but does not foreclose it entirely).
constraint_indexing:constraint_classification(constitutional_authority_boundary__parliamentary_primacy_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_authority_boundary__parliamentary_primacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_authority_boundary__parliamentary_primacy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): The baseline reflects moderate asymmetry between beneficiary and victim positions. The legislature benefits from authority without judicial constraint (arbitrage exit option). Supramajority minorities and permanent minorities cannot exit to judicial protection; they are constrained or trapped. However, extractiveness is not higher (it is not 0.40+, which would characterize it as snare) because: (a) legislatures do negotiate minority rights and protections even under supremacy doctrine, as political necessity and democratic deliberation produce coalition-building; (b) the constraint's legitimacy explicitly rests on democratic principles that include minority voice and amendment procedures; (c) permanent minorities retain non-judicial exit routes (emigration, constitutional amendment, political organizing). The 0.18 value represents a genuine but moderate asymmetry. Suppression (0.35): Moderate-high. The suppression gate is the supermajority requirement for constitutional amendment (in systems with entrenched procedures) or the legislative supermajority needed to revise entrenched protections. Barriers to exit include: (a) institutional capacity (amending constitutions is laborious and politically costly); (b) coalition-building barriers (minorities must persuade majorities to constrain their own authority); (c) normative internalization (many actors accept parliamentary supremacy as legitimate principle, not merely institutional constraint). Suppression is not maximum (0.5+) because amendment remains formally available and has succeeded in many cases; it is not minimal (0.2) because the barriers are genuine and have prevented reform in many contexts. Theater ratio (0.42): The parliamentary procedure creates legitimacy theater — formal debate, amendment process, legislative deliberation all create the appearance of rights protection through democratic procedure. However, the core dynamic (legislature can unilaterally revise constitutional meaning) remains unchanged. Theater is moderate, not high, because legislatures do face real political constraints (need for coalition consensus, electorate preferences) even under supremacy doctrine. Measurements show theater_ratio rising slightly over the 40-year interval (0.38 → 0.45) and extractiveness rising modestly (0.15 → 0.20), reflecting gradual accumulation of rights revisions via supremacy mechanism (legislatures using amendment powers more frequently) and increasing judicialization of constitutional discourse (creating more theater as legislatures must publicly debate and justify rights changes).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence across institutional positions. The legislature sees coordination (Rope) — the constraint enables democratic will implementation. Supramajority minorities see mixed coordination and extraction (Tangled Rope) — legislative deliberation protects some rights but revision remains possible. Permanent minorities see pure extraction (Snare) — rights protection entirely contingent on legislative favor with no exit mechanism. The judiciary sees degradation (Piton) — institutional role reduced to ceremonial interpretation. The analytical observer risks naturalizing the arrangement (Mountain) — seeing parliamentary supremacy as inevitable or natural. Constitutional reform movements see a revisable institutional choice (Scaffold) — the constraint has a sunset via democratic amendment. The perspectival gap between legislative beneficiaries (Rope) and permanent minorities (Snare) is maximal: the constraint that solves coordination for majorities creates pure extraction for excluded groups.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality (d) from beneficiary/victim status and exit options. The legislature is declared as beneficiary with arbitrage exit (can redefine constitutional meaning through legislation, can create new bodies, can establish new procedures) → d ≈ 0.15 (low, approaching full beneficiary) → f(d) ≈ negative (around -0.01), meaning the legislature experiences negative effective extraction (the constraint subsidizes them). Supramajority minorities are victims with constrained exit (face majoritarian revision risk; can appeal to constitutional amendment but at high cost) → d ≈ 0.55 (moderate-high) → f(d) ≈ 0.75, producing moderate experienced extraction. Permanent minorities are victims with trapped exit (no legislative power, no judicial appeal, no affordable exit to other jurisdictions) → d ≈ 0.95 (near-maximal target) → f(d) ≈ 1.42, producing high experienced extraction. The judiciary is constrained (cannot exit from supremacy doctrine without legislative action) → d ≈ 0.45 (moderate), but with low base extractiveness (they are constrained, not extracted-from for benefit of others) → low chi overall. The directionality spread (0.15 to 0.95) produces the perspectival gap from Rope to Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution in parliamentary supremacy: The constraint avoids mandatrophy (becoming indeterminate between coordination and extraction) by maintaining low extractiveness (0.18) and clear suppression (0.35) with moderate theater (0.42). The democratic legitimacy principle (that electoral majorities should implement mandates) is the coordination function — this is genuinely shared by beneficiary and victim perspectives. Majorities do negotiate minority protection as political necessity, not merely as theater. The extraction component (inability to appeal majoritarian revision to judicial review) is asymmetric but not overwhelming, because exit routes remain (amendment, political organizing) and because legislative protection is real, not purely theatrical. The constraint classifies as Tangled Rope from multiple perspectives precisely because it genuinely coordinates democratic will while simultaneously extracting protection against revision. A higher extractiveness (0.40+) would indicate pure majoritarian dominance with no real legislative negotiation of minority rights; lower extractiveness (0.05) would indicate genuine coordinate power-sharing. The 0.18 value reflects historical observation that legislatures under supremacy doctrine do in fact negotiate minority rights and accommodate preferences beyond bare majoritarian will.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_versus_institutional_choice,
    'Is parliamentary supremacy a kernel — a stabilized commitment that grounds the authority structure''s legitimacy — or merely an institutional choice available for democratic revision?',
    'Historical and comparative analysis: (a) Can the parliament constitutionally revise the supremacy doctrine itself? (b) Do constitutional texts subordinate themselves to legislative authority explicitly, or is this an interpretive layer? (c) What happens when legislatures attempt to entrench their own supremacy against future reversal? (d) Do sibling readings (judicial supremacy, coordinate construction) remain live options within the same constitutional tradition?',
    'If kernel: the supremacy doctrine is a foundational commitment from which the authority structure derives legitimacy; erosion of parliamentary primacy is a threat to the legal system itself. If institutional choice: the doctrine is revisable, and sibling readings are coequal options for constitutional reconstruction. This determination affects whether the constraint is a mountain (if kernel) or a tangled_rope with scaffold potential (if institutional choice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_versus_institutional_choice, conceptual, 'Whether parliamentary supremacy is a foundational kernel or revisable institutional choice').

omega_variable(
    suppression_mechanism_structural_versus_normative,
    'What portion of the measured suppression (0.35) is structural (legislative supermajority gate, institutional capacity constraints) versus normative (internalized acceptance of parliamentary authority as legitimate)?',
    'Empirical observation: Do actors attempt constitutional revision despite high barriers? Do reform movements frame supremacy as illegitimate or as legitimate-but-revisable? Do countries with entrenched judicial review retain parliamentary supremacy doctrines in specific domains (e.g., welfare legislation vs rights protection)? What do successful constitutional amendments (superseding parliamentary primacy in specified areas) reveal about the distribution of suppression?',
    'If suppression is primarily structural (supermajority gate, legislative capacity): the constraint remains extractive for permanent minorities even under reform movements, because structural barriers are high. If suppression is partly normative (internalized acceptance of legitimacy): educational and discursive challenges to supremacy doctrine can reduce experienced suppression below the formal institutional gate. This affects the snare vs tangled_rope classification for permanent minorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_versus_normative, empirical, 'Structural versus normative components of suppression in parliamentary supremacy').

omega_variable(
    majoritarian_extraction_versus_legitimate_protection,
    'How do we distinguish legitimate legislative majoritarian protection of minority rights (coordination function) from majoritarian extraction through rights revision?',
    'Case study analysis: (a) Do legislative majorities revise rights in ways that track demographic change (legitimate political adaptation) or in ways that target permanent minorities (extraction)? (b) Do supramajority-dependent areas (confederation, entrenchment in other democracies) show different rights-stability profiles than simple-majority-dependent areas? (c) What is the historical frequency of legislative revision of core rights across different constitutional systems? (d) Do groups with strong legislative representation show higher or lower rates of rights revision than excluded groups?',
    'If extraction is high (frequent majoritarian revision targets minorities): tangled_rope classification shifts toward snare for persistent minorities. If coordination is high (legislatures rarely revise rights, especially for politically weak groups): snare classification downshifts toward tangled_rope. This empirical determination directly affects the measured extractiveness baseline (0.18 is a blended assumption; the actual value depends on historical patterns of legislative rights revision).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_extraction_versus_legitimate_protection, empirical, 'Empirical frequency and directionality of legislative rights revision').

omega_variable(
    coordinate_construction_foreclosure,
    'Does the parliamentary primacy axiom logically foreclose the coordinate construction reading (where legislature, judiciary, and executive hold coordinate authority to interpret the constitution)?',
    'Logical and institutional analysis: (a) Can a legal system hold both ''parliament is supreme'' and ''courts have equal authority to interpret the constitution'' simultaneously without contradiction? (b) How do systems with coordinate construction (e.g., Canada post-Charter, some Federal systems) relate to parliamentary sovereignty doctrines? (c) What is the functional difference between ''parliament can override judicial review'' (supremacy compatible) and ''parliament and courts both have final authority'' (supremacy incompatible)?',
    'If foreclosed: the reading relation from parliamentary_primacy to coordinate_construction is ''forecloses''. If not foreclosed: the relation is ''coexists_with'' (because legislatures can entrench coordinate construction through constitutional amendment, making both readings available at different constitutional moments). The relation type affects the cs_structure.reading_relations declaration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinate_construction_foreclosure, conceptual, 'Whether parliamentary supremacy logically forecloses coordinate construction').

omega_variable(
    judicial_review_incompatibility,
    'Is strong-form judicial review (courts can override legislation on constitutional grounds) logically foreclosed by parliamentary supremacy, or is weak-form review (courts interpret but legislature can reverse via ordinary or entrenched legislation) compatible?',
    'Doctrinal analysis: (a) Do systems with parliamentary supremacy doctrine (UK, Australia historically) permit weak-form judicial review? (b) What happens when legislatures attempt to entrench a statute against judicial review — does this rest on or contradict supremacy doctrine? (c) Can ''parliament is supreme'' coexist with ''courts have presumptive interpretive authority unless reversed'' (the weak form)?',
    'If strong-form review is foreclosed but weak-form is compatible: the supremacy doctrine is narrower than sometimes stated — it forecloses only judicial veto, not judicial interpretation. If weak-form is also foreclosed: supremacy is maximalist (courts are purely advisory). This affects the theater_ratio baseline (0.42 assumes weak-form review remains; maximalist suppression would raise theater via ceremonial judicial role).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_review_incompatibility, conceptual, 'Compatibility of parliamentary supremacy with weak-form judicial review').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conauth_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(conauth_tr_t20, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(conauth_tr_t40, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(conauth_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(conauth_be_t20, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(conauth_be_t40, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 40, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% The constitutional authority boundary kernel decomposes into three distinct constraints, each instantiating one reading. The parliamentary_primacy_reading (this file, ε=0.18) models the supremacy doctrine's structure: moderate extractiveness for permanent minorities, genuine coordination for majorities, institutional degradation of courts. The judicial_supremacy_reading (ε varies, typically 0.25-0.35) models the inverse: judiciary as beneficiary, legislative authority constrained, different extraction dynamics for permanent minorities. The coordinate_construction_reading (ε varies, typically 0.12-0.22) models distributed authority with mutual checking. All three share the same base metrics only if 'constitutional authority' is measured identically; the actual empirical structures differ (different suppression profiles, different theater ratios, different beneficiary/victim sets). These are not the same constraint viewed from three angles — they are three structurally distinct constraints generated by different readings of the kernel. Each should be compiled separately and linked via network.affects_constraints to show family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
