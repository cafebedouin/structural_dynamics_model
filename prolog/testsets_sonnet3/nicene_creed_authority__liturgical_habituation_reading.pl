% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: The Nicene Creed as Liturgical Boundary Marker (Habituation Reading)
 *   domain: Systematic Theology / Ecclesiology / History of Christian Doctrine
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested Nicene Creed kernel:
 *   the liturgical habituation reading, which treats the creed's operative
 *   function as identity-boundary performance through communal recitation,
 *   structurally independent of whether individual worshippers hold the
 *   fourth-century trinitarian ontology in cognitive assent. On this reading,
 *   the creed coordinates a genuine social good — recognizable, continuous
 *   communal identity across a vast, doctrinally heterogeneous,
 *   geographically dispersed body — at very low coercive cost. This is NOT
 *   the strict orthodox reading (which treats the creed as binding
 *   metaphysical doctrine enforced against heresy) nor the symbolic
 *   confessional reading (which locates authority in community discernment
 *   and personal faith). Those are separate constraints, authored separately,
 *   linked here by network reference. The habituation reading's ε is
 *   intentionally very low (0.08) because, by its own lights, what is being
 *   coordinated is shared performance, not doctrinal compliance — the
 *   coercive apparatus that would show up in the strict orthodox reading's ε
 *   is simply not part of what this reading is about.
 *
 * KEY AGENTS:
 *   - worshipping_congregations: primary beneficiary (moderate/mobile) — gain communal identity through recitation
 *   - liturgical_communities: agenda-setter and beneficiary (organized/mobile) — administer the recited form
 *   - ecumenical_bodies: beneficiary (institutional/constrained) — leverage shared recitation as common ground
 *   - dissenting_theologians: excluded voice (moderate/constrained) — object that assent-free recitation launders disciplinary function
 *   - converts_and_new_members: beneficiary/payer (powerless/mobile) — gain belonging at low initial cost
 *   - theological_historians: analytical observer — document the sociological function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.12).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "The Nicene Creed as Liturgical Boundary Marker (Habituation Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "Systematic Theology / Ecclesiology / History of Christian Doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, 'ccb75f35-cb10-4047-a63c-ba6c9d8f752a').
narrative_ontology:cs_kernel_codification('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', fixed_text).
narrative_ontology:cs_authority_grounding('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', practice).
narrative_ontology:cs_interpretation_layer_present('ccb75f35-cb10-4047-a63c-ba6c9d8f752a').
narrative_ontology:cs_reading_relation('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_axiom('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', foundational, identity_constituted_by_performance_not_assent).
narrative_ontology:cs_axiom_status(identity_constituted_by_performance_not_assent, holdable).
narrative_ontology:cs_axiom_grounding('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', identity_constituted_by_performance_not_assent, conventional).
narrative_ontology:cs_axiom('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', secondary, communal_boundary_marking_is_creeds_primary_operative_function).
narrative_ontology:cs_axiom_status(communal_boundary_marking_is_creeds_primary_operative_function, holdable).
narrative_ontology:cs_axiom_grounding('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', communal_boundary_marking_is_creeds_primary_operative_function, empirically_contingent).
narrative_ontology:cs_reference_frame('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', communal_recitation_as_identity_practice).
narrative_ontology:cs_drift_state('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', contemporary_pluralist_liturgy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ccb75f35-cb10-4047-a63c-ba6c9d8f752a', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, worshipping_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_communities).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, ecumenical_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, converts_and_new_members).
narrative_ontology:constraint_victim(nicene_creed_authority__liturgical_habituation_reading, converts_and_new_members).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, communal_identity_through_shared_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recite the creed communally in the liturgy as a shared act binding the assembly together across time and place. Most participants have never parsed the homoousios controversy in detail; the recitation functions as a marker of belonging to this tradition rather than a cognitive assent test. They can move between congregations, denominations, or drop liturgical practice with only social friction, not structural lock-in.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, worshipping_congregations, beneficiary,
    moderate, generational, mobile, global).

% Maintain the recited creed as a stable liturgical text across generations, preserving a recognizable form of communal worship that survives translation, schism, and doctrinal dispute. They administer the practice of recitation itself, distinct from adjudicating its metaphysical content.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_communities, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, liturgical_communities, agenda_setter).

% Point to shared creedal recitation across otherwise divided traditions (Catholic, Orthodox, many Protestant bodies) as evidence of underlying unity that survives doctrinal disagreement about what the words metaphysically commit one to. The shared performance is leveraged as common ground for dialogue.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ecumenical_bodies, beneficiary,
    institutional, civilizational, constrained, global).

% Hold that recitation without metaphysical assent is either dishonest performance or evidence the creed's real function is disciplinary rather than communal — their objection that liturgical framing launders a coercive doctrinal test is not centered in the habituation reading's own account of what the practice does.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, dissenting_theologians, excluded,
    moderate, biographical, constrained, national).

% Learn the creed as part of induction into the community, often before or without full comprehension of its trinitarian technicalities. They gain immediate belonging and participation; the minor cost is the initial unfamiliarity and occasional social pressure to recite words whose content they have not yet examined.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, converts_and_new_members, beneficiary,
    powerless, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, converts_and_new_members, payer).

% Study how creedal recitation has functioned as social glue across councils, schisms, and reformations, largely independent of whether individual worshippers held the intended fourth-century ontology in mind. They document the boundary-marking function as a sociological fact distinct from the theological claim's truth-value.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, theological_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Recitation of a single, stable liturgical text lets a large, dispersed, doctrinally heterogeneous body of worshippers perform shared identity and mutual recognition without requiring uniform metaphysical comprehension or agreement — the coordination problem solved is 'how does a community know its own members' across time and geography, not 'how do we verify correct belief.'
% TRANSFER_FUNCTION: Moves very little materially; what is transferred is symbolic — a sense of belonging and continuity is extended to anyone who performs the recitation, and communities gain a low-cost, low-friction marker for distinguishing insiders from outsiders without doctrinal litigation.
% ABSENT_VOICES: Dissenting theologians who regard recitation-without-assent as either quiet dishonesty or evidence of disciplinary function papered over by ritual are not centered here; their objection belongs properly to the sibling readings (strict_orthodox and symbolic_confessional) which stake claims about what the words must mean.
% DISAPPEARANCE_RATIONALE: If communal creedal recitation vanished, liturgical communities would lose a low-cost unity marker and ecumenical bodies would lose a point of common ground — some rearrangement would follow. But because this reading holds the function is substrate-level (identity performance) rather than metaphysically load-bearing, many worshippers argue the underlying community bonds would persist through other shared practices; the verdict is disputed even within the habituation reading's own adherents.
% FOUNDING_PROBLEM: Early conciliar Christianity needed a shared, repeatable form of words that could travel across languages, regions, and generations to let dispersed congregations recognize one another as the same body, especially as the faith spread beyond any single city's oral catechesis.
% FOUNDING_PROBLEM_CORROBORATION: Historians of liturgy (outside any single denomination's beneficiary interest) attest that creedal recitation continues to function as a boundary-and-belonging marker in ecumenical contexts today, citing continued use in traditions that have long since stopped enforcing the fourth-century metaphysical content as a live test of orthodoxy.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, contested).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.08) because, under this reading, no party extracts rent through the recitation act itself — no sanctions are levied for imperfect comprehension, and no material transfer occurs. Suppression is likewise low (0.12): recitation is expected, not coerced, and exit from liturgical participation carries social rather than structural cost. Theater ratio is moderate and rising (0.20→0.40) because a meaningful share of the practice's persistence is performative continuity — the words are said whether or not their metaphysical weight is examined, which is precisely the phenomenon this reading names as its subject rather than treating as a defect. Accessibility collapse is low (0.20): worshippers who want to examine, reinterpret, or ignore the metaphysical content retain real room to do so without leaving the practice.
 *
 * PERSPECTIVAL GAP:
 *   From the liturgical-communities seat, the recited creed is coordination infrastructure — a shared form enabling recognition across an otherwise fragmented body. From the dissenting-theologians seat (excluded here, centered in the sibling readings), the same recitation looks like the social substrate that makes doctrinal enforcement possible elsewhere: performance without assent is what allows both a coercive orthodox test and a permissive pluralist reinterpretation to ride on the same words. This story does not adjudicate that dispute — it authors the coordination-only face of the kernel and lets the siblings carry the coercive and confessional faces.
 *
 * DIRECTIONALITY LOGIC:
 *   Worshipping congregations, converts, and liturgical communities are authored near the beneficiary end: they gain low-cost belonging and continuity, and their exit options (mobile) mean the practice cannot trap them structurally. Ecumenical bodies sit slightly more constrained because institutional reputational capital is invested in the shared recitation continuing to function as common ground. No victim group is declared in this reading because, by its own account, the recitation-without-assent function does not identify anyone as bearing an asymmetric structural cost — that cost, if it exists, belongs to the strict orthodox reading's coercive apparatus, not to this one.
 *
 * MANDATROPHY ANALYSIS:
 *   The habituation reading resists mandatrophy by keeping the founding problem (dispersed communities need a repeatable recognition marker) tightly scoped to what recitation actually does — establish and maintain shared identity — rather than smuggling in a metaphysical compliance function that would require active enforcement to sustain. Because no beneficiary here collects coercive rents, this reading should not drift into tangled_rope territory unless a future measurement shows recitation becoming a gatekeeping test with real sanctions, at which point the story would need to be re-authored (or the sibling strict_orthodox reading would be the more accurate lens for that dynamic).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_versus_assent_separability,
    'Is liturgical performance of the creed genuinely separable from metaphysical assent as a matter of social fact, or does sustained recitation without assent function as a slow vector for doctrinal enforcement (making this reading''s low ε an artifact of bracketing rather than a true structural fact)?',
    'Longitudinal sociological study of congregations that recite the creed alongside catechetical materials that either do or do not require metaphysical comprehension; compare disciplinary outcomes for members who publicly disavow the metaphysical content while continuing to recite.',
    'If recitation without assent is shown to reliably trigger informal social sanction once disavowal becomes known, the coordination-only framing understates suppression, and this reading''s ε and suppression scores would need revision upward — potentially collapsing the distinction from the strict_orthodox_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_versus_assent_separability, empirical, 'Whether liturgical performance is truly decoupled from metaphysical enforcement or merely a lower-visibility enforcement channel.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the creed''s identity-boundary function best modeled as a genuinely separate coordination-only constraint, or is the decomposition into three readings itself masking a single underlying extractive structure that different observers describe with different vocabularies?',
    'Cross-reading comparison: if the strict_orthodox_reading and this reading, when their stakeholder sets and enforcement data are compared, turn out to describe the same underlying enforcement apparatus using different vocabularies, that would suggest the decomposition is a labeling artifact rather than a structural one.',
    'If the three readings collapse into one underlying extractive structure, ε-invariance is violated and the correct response is re-decomposition, not a single averaged ε. If they remain genuinely structurally distinct (different beneficiaries, different enforcement mechanisms, different victim sets), the three-way decomposition stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three-reading kernel decomposition tracks real structural differences or is an artifact of interpretive framing.').

omega_variable(
    theater_ratio_trajectory_meaning,
    'Does the rising theater_ratio (0.20 to 0.40) indicate healthy adaptation (the creed''s coordination function persisting even as literal metaphysical comprehension declines) or early-stage mandatrophy (a founding problem quietly going dead while the practice persists on inertia)?',
    'Compare congregational retention and reported sense-of-belonging outcomes in communities with high versus low theater_ratio trajectories; if belonging outcomes remain stable or improve as literal comprehension declines, that supports the adaptation reading.',
    'If the trajectory indicates dead-founding-problem drift, this reading approaches piton territory (performance persisting without a live coordination function); if it indicates healthy adaptation, the rope classification is robust to declining comprehension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_ratio_trajectory_meaning, empirical, 'Whether rising theatricality signals adaptive persistence or founding-problem obsolescence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nice_tr_t20, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(nice_tr_t40, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(nice_tr_t60, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(nice_tr_t80, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 80, 0.37).
narrative_ontology:measurement(nice_tr_t100, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(nice_be_t20, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(nice_be_t40, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(nice_be_t60, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 60, 0.07).
narrative_ontology:measurement(nice_be_t80, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 80, 0.08).
narrative_ontology:measurement(nice_be_t100, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 100, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nicene_creed_authority__liturgical_habituation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__liturgical_habituation_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% Three-story decomposition of the nicene_creed_authority kernel per the ε-invariance principle: liturgical_habituation_reading (this story, rope, ε≈0.08, coordination via shared performance), strict_orthodox_reading (expected tangled_rope or snare, higher ε, coercive doctrinal enforcement with named heresy sanctions), symbolic_confessional_reading (expected rope or scaffold, moderate ε, authority grounded in community discernment rather than fixed metaphysics). This story provides the social substrate (habituated shared performance) that the other two readings interpret and, in the orthodox case, potentially weaponize; the habituation reading's low ε and lack of enforcement should not be read as evidence against the higher ε authored in the orthodox sibling — they describe different structural claims sharing one liturgical text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
