% ============================================================================
% CONSTRAINT STORY: federal_construction__bundesrat_entanglement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_construction__bundesrat_entanglement, []).

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
 *   constraint_id: federal_construction__bundesrat_entanglement
 *   human_readable: Bundesrat Entanglement: Land Executives as Co-Legislators
 *   domain: legal/constitutional_federalism
 *
 * SUMMARY:
 *   The Bundesrat represents a structural choice in German federalism: rather
 *   than separating federal and Land legislative spheres, the Basic Law puts
 *   Land executives inside the federal lawmaking apparatus. Senators
 *   (Bundesrat members) are delegates of Land governments, voting under
 *   instruction from their home executives. For ~50% of federal legislation
 *   (consent matters affecting Land competences, taxes, and administration),
 *   Bundesrat approval is mandatory. This creates a constitutional
 *   architecture of entangled administrations rather than separated powers.
 *   The constraint exhibits the core tension: the entanglement mechanism is
 *   justified as coordination (federal legislation affecting Länder must be
 *   negotiated with regional input) but operates as extraction (Land
 *   executives gain unilateral blocking power over federal legislation,
 *   extracting concessions unrelated to the original bill). The measurement
 *   trajectory shows increasing extractiveness and suppression from 1949 to
 *   2013, reflecting the accumulation of joint tasks and shared financing
 *   mechanisms that deepen entanglement without addressing accountability
 *   collapse. This reading instantiates one specific claim about this kernel:
 *   that the Bundesrat's co-legislative architecture is the core structural
 *   mechanism, producing beneficiaries (Land executives) and victims (clear
 *   accountability lines). Sibling readings emphasize different aspects: the
 *   cooperative_drift reading highlights how shared financing deepened
 *   entanglement; the lander_cultural_sovereignty reading emphasizes Land
 *   protection of cultural autonomy. All three readings are grounded in the
 *   same constitutional text (the Basic Law) but parse it through different
 *   frameworks.
 *
 * KEY AGENTS:
 *   - Land Executives (as Bundesrat Members): Primary beneficiary (institutional/arbitrage) — gain co-legislative veto power over federal legislation; can extract concessions unrelated to the triggering bill
 *   - Clear Accountability Lines: Primary victim (powerless/trapped) — cannot exit the entanglement; citizens and courts cannot assign responsibility for policy failures to a single level
 *   - Federal Government (Chancellor/Administration): Secondary actor (institutional/constrained) — benefits from coordination legitimacy but bears cost of legislative deadlock and perpetual negotiation
 *   - Citizen Voters: Secondary victim (moderate/constrained) — cannot punish electoral failure because responsibility is distributed; democratic accountability mechanism fails
 *   - Smaller Länder (Demographic Minorities): Tertiary beneficiary (organized/constrained) — extract disproportionate veto leverage through bloc-voting in Bundesrat; weaker Länder subsidize stronger ones through concession extraction
 *   - Constitutional Court: Tertiary actor (institutional/analytical) — interprets Bundesrat scope and consent requirements; reinforces or redefines the entanglement mechanism through review
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_construction__bundesrat_entanglement, 0.52).
domain_priors:suppression_score(federal_construction__bundesrat_entanglement, 0.58).
domain_priors:theater_ratio(federal_construction__bundesrat_entanglement, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_construction__bundesrat_entanglement, extractiveness, 0.52).
narrative_ontology:constraint_metric(federal_construction__bundesrat_entanglement, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federal_construction__bundesrat_entanglement, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_construction__bundesrat_entanglement, tangled_rope).
narrative_ontology:human_readable(federal_construction__bundesrat_entanglement, "Bundesrat Entanglement: Land Executives as Co-Legislators").
narrative_ontology:topic_domain(federal_construction__bundesrat_entanglement, "legal/constitutional_federalism").

domain_priors:requires_active_enforcement(federal_construction__bundesrat_entanglement).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federal_construction__bundesrat_entanglement, '5493c082-8064-4d99-a8b2-d2c646a2d218').
narrative_ontology:cs_kernel_codification('5493c082-8064-4d99-a8b2-d2c646a2d218', formalized).
narrative_ontology:cs_authority_grounding('5493c082-8064-4d99-a8b2-d2c646a2d218', lineage).
narrative_ontology:cs_interpretation_layer_present('5493c082-8064-4d99-a8b2-d2c646a2d218').
narrative_ontology:cs_reading_relation('5493c082-8064-4d99-a8b2-d2c646a2d218', federal_construction__cooperative_drift_reading, coexists_with).
narrative_ontology:cs_reading_relation('5493c082-8064-4d99-a8b2-d2c646a2d218', federal_construction__lander_cultural_sovereignty, coexists_with).
narrative_ontology:cs_axiom('5493c082-8064-4d99-a8b2-d2c646a2d218', foundational, executive_representation_in_lawmaking_structurally_entangles_levels).
narrative_ontology:cs_axiom_status(executive_representation_in_lawmaking_structurally_entangles_levels, holdable).
narrative_ontology:cs_axiom_grounding('5493c082-8064-4d99-a8b2-d2c646a2d218', executive_representation_in_lawmaking_structurally_entangles_levels, empirically_contingent).
narrative_ontology:cs_axiom('5493c082-8064-4d99-a8b2-d2c646a2d218', secondary, accountability_requires_singular_attribution_of_legislative_authority).
narrative_ontology:cs_axiom_status(accountability_requires_singular_attribution_of_legislative_authority, holdable).
narrative_ontology:cs_axiom_grounding('5493c082-8064-4d99-a8b2-d2c646a2d218', accountability_requires_singular_attribution_of_legislative_authority, deontological).
narrative_ontology:cs_reference_frame('5493c082-8064-4d99-a8b2-d2c646a2d218', separated_legislative_spheres).
narrative_ontology:cs_drift_state('5493c082-8064-4d99-a8b2-d2c646a2d218', contemporary_post_reunification, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5493c082-8064-4d99-a8b2-d2c646a2d218', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(federal_construction__bundesrat_entanglement, federal_construction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_construction__bundesrat_entanglement, land_executives).
narrative_ontology:constraint_victim(federal_construction__bundesrat_entanglement, clear_accountability_lines).
narrative_ontology:constraint_victim(federal_construction__bundesrat_entanglement, federal_legislative_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLEAR ACCOUNTABILITY STRUCTURE (SNARE) — Cannot exit the entanglement. Citizens and courts cannot identify a single responsible legislator when policy fails. Accountability diffuses across federal and Land levels, each able to blame the other. Zero degrees of freedom; extracted benefit is governmental discretion in evasion.
constraint_indexing:constraint_classification(federal_construction__bundesrat_entanglement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALLER LÄNDER IN COALITION (TANGLED ROPE) — Constrained by demographic and economic weight, but gain disproportionate veto power through Bundesrat supermajority requirements. Genuine coordination function: federal legislation affecting their citizens must accommodate regional variance. But asymmetric extraction: weaker Länder extract federal concessions through blocking power, which stronger Länder subsidize. Mixed: both coordination benefit and extractive leverage.
constraint_indexing:constraint_classification(federal_construction__bundesrat_entanglement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STRONG LAND EXECUTIVE (ROPE) — Institutional actor with arbitrage options (can negotiate bilaterally with federal government, can threaten constitutional review, can implement differentiated regulation within their territory). Experiences the Bundesrat as a pure coordination mechanism: legislation that affects Bavarian competences must be negotiated, allowing the Land to preserve its policy autonomy. Net beneficiary — extraction flows toward the Land, not away.
constraint_indexing:constraint_classification(federal_construction__bundesrat_entanglement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CITIZEN VOTER (SNARE) — Constrained by structural inability to assign blame when policy fails. Bundesrat entanglement means no election can change a single policy vector because responsibility is distributed across federal and Land officials. High suppression: democratic accountability mechanism fails. Extraction: governments extract discretion to ignore electoral signals because voters cannot punish any single level.
constraint_indexing:constraint_classification(federal_construction__bundesrat_entanglement, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL GOVERNMENT (TANGLED ROPE) — Constrained by Bundesrat veto on ~50% of legislation (consent matters: taxes, administration, land use). Coordination function is genuine: regional variance and local knowledge improve policy. But asymmetric extraction: federal agenda is perpetually negotiated, and Land executives extract federal concessions in exchange for Bundesrat votes. The federal government benefits from legitimacy of consensual federalism but bears the cost of legislative deadlock.
constraint_indexing:constraint_classification(federal_construction__bundesrat_entanglement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL IMMUTABILITY (MOUNTAIN) — From a civilizational perspective, the Basic Law's federalism structure is presented as an irreducible constitutional commitment: Germany is constitutionally committed to a Bundesrat with veto power over Land-affecting legislation. This perspective sees entanglement as a natural law of German constitutional design — immutable short of constitutional amendment. However, the structural data contradicts this: the entanglement mechanism benefits identifiable Land executives and harms clear accountability, indicating that the 'constitutional immutability' framing naturalizes a contingent institutional arrangement. Engine will flag as false summit.
constraint_indexing:constraint_classification(federal_construction__bundesrat_entanglement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_construction__bundesrat_entanglement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_construction__bundesrat_entanglement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_construction__bundesrat_entanglement, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_construction__bundesrat_entanglement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federal_construction__bundesrat_entanglement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Bundesrat mechanism extracts value for Land executives in the form of co-legislative veto power. The measurement trajectory (0.35 → 0.52 over 64 years) reflects accumulating extraction as joint tasks and shared financing deepened federal-Land interdependence. The value is not extreme (not 0.70+) because genuine coordination benefits exist — federal legislation affecting Länder does benefit from regional input. The extraction occurs through asymmetry: Land executives use Bundesrat veto on consent matters to extract concessions on unrelated bills, creating a coordination mechanism that is partly genuine (consensus improves policy legitimacy) and partly extractive (veto power enables legislative hijacking). Suppression (0.58): Moderate-high. The core suppression is accountability collapse: entanglement prevents citizens from assigning responsibility for policy failures. Measurement shows rising suppression (0.42 → 0.58) as complexity increased. The suppression is structural (citizens face material difficulty assigning blame across entangled levels) and enforced (constitutional law mandates this structure; no single-level reform is possible without constitutional amendment). Theater ratio (0.48): Moderate. The Bundesrat process includes performative elements (formal voting, consultative phases) but also genuine negotiation of substantive policy conflicts. The theater has increased over time as consensus-seeking rituals have formalized (0.38 → 0.48) while the underlying constraint remains unchanged. Theater is lower than in institutions that are purely performative (e.g., advisory bodies) because Bundesrat votes have genuine legal effect.
 *
 * PERSPECTIVAL GAP:
 *   The largest gaps appear between the federal government's rope classification and citizen voters' snare classification. From the federal perspective, the Bundesrat is a coordination mechanism: federal legislation must accommodate regional variance, which improves legitimacy and policy fit. From the citizen perspective, the same mechanism produces accountability collapse: no one can be held responsible for failures because all levels claim their hand was forced by the others. The Land executive perspective (rope) sees the Bundesrat as a protection mechanism — guaranteeing that federal legislation affecting Land competences cannot be imposed unilaterally. The accountability perspective (snare) sees the same mechanism as a protection mechanism that disables democratic punishment. These gaps reflect real structural differences: the beneficiary (Land executives) and victims (accountability lines) occupy incommensurable positions relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each institutional perspective's directionality (d) derives from its structural position in the extraction flow. Land executives (beneficiary + institutional power + arbitrage options) experience low d → negative effective extraction. Federal government (mixed beneficiary/victim + institutional power + constrained options) experiences moderate d. Citizens (victims + moderate power + constrained options) experience high d. The citizen perspective (Perspective 4) shows higher d than the federal government (Perspective 5) because citizens have less organizational capacity to negotiate within the entanglement. Small Länder (Perspective 2) occupy a strategic position: demographically weaker (victim-like in pure majority voting) but veto-empowered through Bundesrat bloc rules (beneficiary position), producing an intermediate d value that captures both constraint and leverage. The analytical observer (Perspective 6) presents a false summit: the 'constitutional immutability' framing treats contingent institutional arrangements (the decision to place executives inside lawmaking rather than outside it) as natural law. The engine's false summit detector identifies this because identifiable beneficiaries (Land executives) exist and the omega variables document the alternative design space (other federalisms coordinate without executive entanglement).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy because it clearly identifies both coordination and extraction functions. The tangled_rope classification captures both: the Bundesrat mechanism (1) coordinates federal-Land policy to improve legitimacy and regional fit, and (2) extracts legislative concessions for Land executives through veto power. The measurement trajectory shows that as entanglement deepened (joint tasks, shared financing), the extraction component rose while the coordination component remained stable. The classification from the citizen/accountability perspective (snare) is not a mandatrophy collapse but a perspectival gap: the same constraint appears as coordination from the beneficiary view and as pure extraction from the victim view. The false summit danger arises from the analytical perspective that treats entanglement as constitutional immutability (mountain). This risks masking the contingent choice (executives inside vs. outside lawmaking) as natural law. The omegas and comparative evidence (other federalisms coordinate without this mechanism) prevent this collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_natural_law_vs_constructed,
    'Is the Bundesrat''s entanglement mechanism a natural law of federalism (constitutional necessity) or a constructed institutional arrangement that benefits identifiable actors?',
    'Comparative constitutional analysis: Do other federal systems (US, Canada, Austria, Switzerland) achieve federal-regional coordination without executive-branch entanglement? If yes, entanglement is contingent. If no, assess whether the absence reflects genuine structural necessity or path dependence.',
    'If natural law: mountain classification from all perspectives. If constructed: tangled_rope from institutional perspectives, snare from accountability perspectives. The reading''s central claim hinges on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_natural_law_vs_constructed, empirical, 'Whether Bundesrat entanglement is constitutional necessity or institutional contingency').

omega_variable(
    sibling_reading_relationship_cooperative_drift,
    'Does this reading (Bundesrat as structural co-legislation) foreclose, coexist with, or influence the cooperative_drift reading (gradual entanglement through joint tasks and shared financing)?',
    'Historical genealogy: Did the Bundesrat''s co-legislative role precede or follow the emergence of joint tasks and shared taxes? If Bundesrat veto predates shared financing, this reading influences the cooperative_drift reading. If shared financing was the historical driver, cooperative_drift influences this reading.',
    'Relationship determination shapes how the engine models constraint family structure and authority succession.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_relationship_cooperative_drift, empirical, 'Genealogical relationship between Bundesrat co-legislation and cooperative entanglement').

omega_variable(
    sibling_reading_relationship_lander_sovereignty,
    'Does this reading (Bundesrat as co-legislative veto) foreclose, coexist with, or influence the lander_cultural_sovereignty reading (Länder cultural autonomy as inviolable)?',
    'Doctrinal analysis: Does Bundesrat veto power over cultural matters (education, broadcasting) constitute a restriction on Land cultural sovereignty, or an enabling mechanism (protecting Land cultural autonomy against federal overreach)? If restriction: forecloses. If enabling: coexists_with.',
    'Determines whether the two readings occupy incompatible constitutional positions or represent compatible aspects of federalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_relationship_lander_sovereignty, conceptual, 'Whether co-legislative veto restricts or protects Land cultural sovereignty').

omega_variable(
    accountability_measurement_gap,
    'How is clear accountability quantified when governance is entangled across levels? What observable distinguishes ''enough clarity to allocate responsibility'' from ''accountability collapse''?',
    'Electoral and legal study: Track public attribution of policy failures in entangled vs separated federal systems. Compare variance in citizen blame assignment. Examine court willingness to assign constitutional liability to specific levels.',
    'If accountability is recoverable through sophisticated analysis: suppression is lower (voters can attribute if they invest effort). If accountability is genuinely collapsed: suppression remains high. Affects classification magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_measurement_gap, empirical, 'Measurability of clear accountability in entangled federalism').

omega_variable(
    beneficiary_identification_precision,
    'Who precisely benefits from Bundesrat entanglement? All Land executives equally, or disproportionately small/rural Länder with bloc-voting leverage?',
    'Policy outcome analysis: Track which Länder extract concessions through Bundesrat blocking. Correlate with demographic size, party alignment with federal government, and policy divergence from federal preference.',
    'If beneficiary set is narrow (small Länder), extractiveness is higher and more concentrated. If broad (all Länder), coordination function is more genuine. Affects directionality calculations for inter-institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_precision, empirical, 'Distribution of Bundesrat veto benefits across Land executives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_construction__bundesrat_entanglement, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bundesrat_theater_1949, federal_construction__bundesrat_entanglement, theater_ratio, 0, 0.38).
narrative_ontology:measurement(bundesrat_theater_1990, federal_construction__bundesrat_entanglement, theater_ratio, 10, 0.44).
narrative_ontology:measurement(bundesrat_theater_2013, federal_construction__bundesrat_entanglement, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(bundesrat_extractiveness_1949, federal_construction__bundesrat_entanglement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bundesrat_extractiveness_1990, federal_construction__bundesrat_entanglement, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(bundesrat_extractiveness_2013, federal_construction__bundesrat_entanglement, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bundesrat_suppression_1949, federal_construction__bundesrat_entanglement, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bundesrat_suppression_1990, federal_construction__bundesrat_entanglement, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(bundesrat_suppression_2013, federal_construction__bundesrat_entanglement, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_construction__bundesrat_entanglement, enforcement_mechanism).
narrative_ontology:affects_constraint(federal_construction__bundesrat_entanglement, federal_construction__cooperative_drift_reading).
narrative_ontology:affects_constraint(federal_construction__bundesrat_entanglement, federal_construction__lander_cultural_sovereignty).

% DUAL FORMULATION NOTE:
% The Bundesrat entanglement reading is part of a three-reading constraint family unified by the kernel 'federal_construction' in German constitutional law. Each reading emphasizes different mechanisms: (1) bundesrat_entanglement focuses on the co-legislative veto mechanism and its effects on accountability. (2) cooperative_drift_reading emphasizes the historical accumulation of joint tasks and shared financing that deepened institutional interdependence. (3) lander_cultural_sovereignty_reading emphasizes the preservation of Land autonomy in cultural/identity domains. These readings have different ε values and different beneficiary/victim structures because they model different structural mechanisms, even though all address the same constitutional kernel. The network.affects_constraints entries establish family relationships; all three readings are interdependent and should be analyzed together to understand the full federal_construction kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_construction__bundesrat_entanglement, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
