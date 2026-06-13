% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Treaty Primacy as Secession Legitimacy Boundary
 *   domain: political/constitutional/resource
 *
 * SUMMARY:
 *   This constraint instantiates the treaty-primacy reading of the
 *   secession-legitimacy kernel. The reading asserts that Indigenous treaty
 *   rights predate and supersede both federal and provincial authority,
 *   making them binding gates on any territorial rearrangement. Under this
 *   reading, a provincial secession without Indigenous consent is inherently
 *   illegitimate, regardless of referendum majorities or constitutional
 *   amendment (which cannot unilaterally alter treaty relationships without
 *   treaty-holder agreement). The constraint coordinates multiple overlapping
 *   sovereignties (federal, provincial, Indigenous) by establishing a single
 *   binding principle: consent from treaty-holding nations is non-waivable.
 *   This is a tangled rope: it genuinely solves a coordination problem (how
 *   to manage overlapping territorial claims), but it does so by
 *   asymmetrically transferring veto authority to Indigenous nations at the
 *   cost of non-Indigenous provincial autonomy. Active enforcement is
 *   required because sovereigntist movements and provincial governments
 *   contest the constraint continuously, and the constraint persists only
 *   through courts and international bodies upholding treaty primacy.
 *
 * KEY AGENTS:
 *   - Indigenous treaty nations — beneficiary agenda-setter; hold veto over territorial rearrangement; benefit from a legitimacy framework that requires their consent
 *   - Provincial sovereigntist coalitions — payer; seek independence but face mandatory consultation gate and veto risk; bear transaction costs and loss of unilateral self-determination
 *   - Federal government — payer; cannot unilaterally resolve secession without Indigenous consent; bears cost of managing concurrent legitimacy claims
 *   - Non-Indigenous provincial citizens — payer/beneficiary; experience constraint as subordination of majority will, but also benefit from property-right stability and governance continuity
 *   - Treaty interpretation authorities — agenda-setter; courts and commissions enforce or refuse the consent gate through legal rulings
 *   - International legal systems — observer; provide external legitimacy anchors through UNDRIP and ILO; constrain reframing efforts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.72).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Treaty Primacy as Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political/constitutional/resource").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '1b2314d6-231e-4849-aaa9-c1342fb77265').
narrative_ontology:cs_kernel_codification('1b2314d6-231e-4849-aaa9-c1342fb77265', fixed_text).
narrative_ontology:cs_authority_grounding('1b2314d6-231e-4849-aaa9-c1342fb77265', lineage).
narrative_ontology:cs_interpretation_layer_present('1b2314d6-231e-4849-aaa9-c1342fb77265').
narrative_ontology:cs_reading_relation('1b2314d6-231e-4849-aaa9-c1342fb77265', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b2314d6-231e-4849-aaa9-c1342fb77265', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('1b2314d6-231e-4849-aaa9-c1342fb77265', secession_legitimacy_boundary__grievance_threshold_reading, influences).
narrative_ontology:cs_axiom('1b2314d6-231e-4849-aaa9-c1342fb77265', foundational, treaty_rights_predate_confederation).
narrative_ontology:cs_axiom_status(treaty_rights_predate_confederation, holdable).
narrative_ontology:cs_axiom_grounding('1b2314d6-231e-4849-aaa9-c1342fb77265', treaty_rights_predate_confederation, empirically_contingent).
narrative_ontology:cs_axiom('1b2314d6-231e-4849-aaa9-c1342fb77265', foundational, treaty_rights_supersede_provincial_authority).
narrative_ontology:cs_axiom_status(treaty_rights_supersede_provincial_authority, holdable).
narrative_ontology:cs_axiom_grounding('1b2314d6-231e-4849-aaa9-c1342fb77265', treaty_rights_supersede_provincial_authority, deontological).
narrative_ontology:cs_reference_frame('1b2314d6-231e-4849-aaa9-c1342fb77265', treaty_rights_as_binding_sovereignties).
narrative_ontology:cs_drift_state('1b2314d6-231e-4849-aaa9-c1342fb77265', contemporary_securitization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1b2314d6-231e-4849-aaa9-c1342fb77265', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, non_indigenous_secession_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_sovereigntist_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, non_indigenous_provincial_citizens).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, non_indigenous_provincial_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold pre-confederation treaty rights that the reading asserts supersede both federal and provincial authority. They benefit from a legitimacy framework that requires their consent to territorial rearrangement. They co-set the agenda through treaty interpretation, land claims processes, and consultation requirements. Their exit consists of negotiating new agreements or abandoning treaty enforcement claims — neither is viable without massive institutional and political cost.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations, agenda_setter).

% Seek provincial independence but must now contend with a framework that treats Indigenous treaty consent as a binding gate on legitimacy. They view the constraint as subordinating their democratic majority to external veto. They bear the cost of negotiation overhead and loss of unilateral claim to represent territorial self-determination. Their exit from the constraint would be constitutional amendment or treaty supersession — both politically prohibitive.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_sovereigntist_coalitions, payer,
    powerful, biographical, constrained, national).

% Cannot unilaterally settle secession without Indigenous consent under this reading, even in principle. It bears transaction costs of managing concurrent federal and treaty-based legitimacy claims. Its exit is constitutional reform or treaty abrogation — both are high-cost and face organized resistance.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Subject to a constraint that ties their territorial self-determination to external consent. They experience this as subordination of majority will to minority veto (from their framing). They also benefit from the constraint's stabilization of property rights and governance continuity. Their exit is low — they can vote in provincial referenda but cannot unilaterally exit the consent requirement.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, non_indigenous_provincial_citizens, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, non_indigenous_provincial_citizens, beneficiary).

% Courts, treaty commissions, and international bodies interpret what treaty rights require. They enforce (or refuse to enforce) the consent gate through legal rulings. Their control of interpretation is the mechanism that makes the constraint stick. They face pressure from both sovereigntist movements (demanding narrow readings) and Indigenous nations (demanding broad readings).
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, treaty_interpretation_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Provide legitimacy anchors through UNDRIP (UN Declaration on Rights of Indigenous Peoples) and ILO conventions asserting Indigenous self-determination and treaty rights. Their observations create external pressure that supports the treaty-primacy reading and constrain domestic reframing efforts.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_legal_systems, observer,
    institutional, generational, analytical, global).

% Are structurally excluded from the negotiation process when secession affects territories with Indigenous treaty claims. They can mobilize non-Indigenous sentiment but cannot participate in the legitimacy gate itself. Their exclusion is what the enforcement apparatus maintains.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, separatist_movements_without_indigenous_base, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_nations).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates overlapping territorial claims and sovereignty assertions between federal, provincial, and Indigenous treaty jurisdictions by establishing a single rule: no unilateral territorial rearrangement without treaty-holder consent. Solves a genuine coordination problem — absent this rule, three sovereignties claim ultimate authority over the same territory, and conflicts multiply. The rule creates a coherent negotiation structure.
% TRANSFER_FUNCTION: Transfers decision-making authority over secession legitimacy from provincial majorities and federal governments to Indigenous treaty nations, who must affirmatively consent for a secession to be treated as legitimate under this reading. Non-Indigenous populations and sovereigntist movements lose unilateral claim to territorial self-determination; Indigenous nations gain veto power and mandatory consultation.
% ABSENT_VOICES: Separatist movements without Indigenous roots are structurally excluded from setting terms. They can organize and advocate but have no seat at the legitimacy gate. Landless Indigenous peoples (those without specific treaty territories) are partially excluded — the constraint operates through pre-existing treaties, leaving out Indigenous communities whose historical territories fall outside treaty frameworks or whose treaties have been extinguished. Younger generations of non-Indigenous populations see this constraint as inherited, with no fresh referendum on whether to accept it.
% DISAPPEARANCE_RATIONALE: If the treaty-consent gate vanished overnight, provincial referenda could unilaterally determine secession within weeks, property law would shift from dual (provincial + treaty-based) to purely provincial authority, and Indigenous nations would lose their structural veto and mandatory-consultation position. They would return to the less favorable position of lobbying federal authority for recognition. The territorial and jurisdictional map would rearrange entirely.
% FOUNDING_PROBLEM: European confederation left in place complex overlapping territorial claims: federal sovereignty, provincial authority, and pre-confederation Indigenous treaty rights all asserted claims to the same lands. Earlier frameworks (colonial law, royal proclamation precedents) treated treaties as subordinate to state authority. The founding problem was how to establish coherent territorial legitimacy when three sovereignties claimed the same space and treaties were being violated or ignored.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous nations attest the problem persists — they continue to invoke treaty rights and resist unilateral federal/provincial authority. International legal bodies (UNDRIP signatories, ILO bodies) attest the problem is live in contemporary jurisdictions (Canada, Australia, USA). Courts have found treaties operative. Federal and provincial governments acknowledge treaties exist, though they contest their scope. Independent scholars of constitutional law document ongoing treaty-non-compliance. The consensus is wide that the founding problem remains unsolved and actively generates conflict — not from Indigenous advocates alone, but from courts, international bodies, and even state actors' own admission.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.55 to 0.68 over the interval as sovereigntist movements gain organizational strength and encounter the consent gate as a persistent cost, not a temporary procedural step. The trajectory flattens after t=25, suggesting the constraint has reached an equilibrium where the cost is acknowledged but cannot be easily bypassed. Suppression is high (0.72 at endpoint) because the constraint's persistence depends on actively excluding sovereigntist movements from unilateral territorial decisions and on enforcing treaty interpretation against revisionist state readings. Theater grows from 0.22 to 0.41 — much of the enforcement activity becomes ritualized: consultations occur but outcomes are often predetermined; sovereignty discussions invoke treaty rhetoric but focus on minimizing actual concessions; the federal government performs respect for treaties while restructuring around them. The gap between base_extractiveness (0.68) and claimed_type (tangled_rope) is intentional — this reading claims the constraint is coordination with asymmetric extraction, and the metrics support that reading: genuine coordination problem (overlapping sovereignties) + genuine asymmetric extraction (provincial autonomy subordinated to Indigenous veto) + active enforcement required.
 *
 * PERSPECTIVAL GAP:
 *   From the treaty-nation seat, the constraint is a recognition of justice — pre-confederation rights restored to their rightful place, and majority-will cannot override them. This is coordination: legitimate authority shared across sovereignties. From the non-Indigenous sovereigntist seat, it is veto imposition — external authority blocking self-determination, majority will subordinated to historical accident (which ancestors signed which treaties). The theater metric captures this gap: governments perform consultation while trying to minimize its binding force; consultations are ritualized; treaty language is invoked but reinterpreted narrowly. The engine computes per-seat classification from the structural data; sovereigntist-seat directionality should be high (target of suppression, facing veto), while treaty-nation directionality should be low (beneficiary, holder of gate). The divergence in computed type is exactly what the apparatus is designed to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty nations are the structural beneficiaries (d ≈ 0.1–0.2): the constraint gives them veto, mandatory consultation, and recognition as legitimate sovereigns. They have constrained but not trapped exit (they could abandon treaties, renegotiate them, but the cost is extremely high — identity, territorial claims, historical legitimacy all ride on treaty assertion). Sovereigntist movements are the structural targets (d ≈ 0.75–0.85): they face veto, suppression of alternative framings, and loss of unilateral claims. Their exit options are trapped or at best identity-locked (a sovereigntist cannot avoid the constraint without abandoning the sovereigntist project itself). Federal government is a payer (d ≈ 0.65–0.70) — it must manage concurrent claims and faces constrained exit (constitutional reform is possible but politically prohibitive). Non-Indigenous citizens sit near symmetric (d ≈ 0.45–0.55): they face constraints on territorial self-determination but also benefit from legal stability. Treaty interpretation authorities are near the beneficiary end (d ≈ 0.2–0.3): they gain legitimacy and institutional power from administering the constraint, with low exit cost (they could reverse course via new rulings, but the reputational cost is high).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (overlapping sovereignties with no coherent legitimacy gate) is live and actively generates conflict. The constraint genuinely solved it by establishing a binding principle. However, the theater metric (rising from 0.22 to 0.41) and the measurement trajectory (extraction rising then plateauing, suppression stable but high) suggest the constraint is drifting toward performance: consultations are held but often produce predetermined outcomes; treaty language is invoked but reinterpreted narrowly; the federal government appears to respect treaties while restructuring around them. This is not yet mandatrophy (the founding problem is still active, and the constraint still matters), but it is evidence of capture — the constraint is being transformed from a binding gate into a ritual that provides legitimacy cover for state decisions already made. A shift toward higher theater (above 0.5) with stable or falling base_extractiveness would signal true mandatrophy: the constraint maintained theatrically while its functional power erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_scope_ambiguity,
    'Do pre-confederation treaties grant Indigenous nations a binding veto over territorial rearrangement, or do they grant rights and interests that require consultation but not necessarily consent?',
    'Court rulings, treaty commission interpretations, negotiated clarifications between Indigenous nations and federal government, and international legal precedent (UNDRIP interpretations, ILO rulings on Indigenous rights).',
    'If veto is binding, the constraint is a tangled rope with strong structural asymmetry — Indigenous nations hold ultimate gate. If consultation-only, the constraint becomes weaker and less extractive from the sovereigntist perspective; consultation could be satisfied performatively. This directly affects whether the treatment as a binding coordination rule (tangled rope) is correct or whether it should be reclassified as a weaker mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_scope_ambiguity, empirical, 'Scope of treaty rights: veto vs. consultation-only').

omega_variable(
    constitutional_amendment_pathway,
    'Can the federal government unilaterally amend the constitution to override treaties, or do treaties themselves require Indigenous consent to any modification?',
    'Constitutional legal doctrine, court rulings, and comparative federalism analysis. The Canadian Constitution Act 1982 section 35 recognizes Aboriginal rights as constitutionally protected; whether constitutional amendment can touch them without consent is unsettled.',
    'If constitutional amendment alone can override treaties, the treaty-primacy reading is not as strong as claimed — federal authority could theoretically bypass Indigenous consent through constitutional reform. If treaties are entrenched beyond amendment, the reading is much stronger. This affects the measured suppression (how much enforcement machinery is actually required to keep the constraint in place).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_amendment_pathway, empirical, 'Whether constitutional amendment can unilaterally override treaties').

omega_variable(
    treaty_nation_internal_heterogeneity,
    'Do all Indigenous nations with historical treaty claims agree on the veto principle, or is there internal disagreement among Indigenous peoples about treaty assertion and sovereignty claims?',
    'Documented positions of specific treaty nations, internal debates within Indigenous governance structures, and consensus statements from Indigenous legal forums.',
    'If Indigenous nations are internally divided on whether to assert treaty veto or to negotiate alternative arrangements, the constraint is less coherent — the beneficiary set is fractured. This could lower extraction metrics (less unified beneficiary power) and suggest the constraint is contested even among those it nominally benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_nation_internal_heterogeneity, empirical, 'Coherence of Indigenous-nation consensus on treaty veto assertion').

omega_variable(
    territoriality_of_treaty_constraint,
    'Does the treaty-consent requirement apply to all provincial territory or only to lands explicitly covered by specific treaties? How are disputes over treaty territory settled?',
    'Land claims commissions, court rulings on treaty boundaries, and negotiated agreements clarifying which territories are treaty-bound.',
    'If the constraint applies only to a subset of provincial territory (say, 15-30% that is explicitly treaty-covered), the effective scope and extractiveness may be lower than measured here. If it applies province-wide through broad interpretations of historical sovereignty, extractiveness is higher. This affects the spatial_scope assessment and the magnitude of the constraint''s practical impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territoriality_of_treaty_constraint, empirical, 'Territorial scope of treaty-based veto claims').

omega_variable(
    reading_vs_naturalness_ambiguity,
    'Is the treaty-primacy principle itself a natural fact of law (inherent in the concept of a treaty), or is it a reading that competes with other legitimate readings of the same constitutional framework?',
    'Comparative constitutional analysis (how do other federal states with Indigenous treaty histories handle this?), international legal consensus (UNDRIP interpretation), and the trajectory of case law (are courts converging on treaty primacy or remaining divided?).',
    'If treaty primacy is a natural law of treaty interpretation, it should be classified as a mountain (unchangeable, pre-existing). If it is one reading among several competing with equal legitimacy, it is a constraint that could be unmade by rereading the kernel. The current classification as tangled rope assumes the latter — it is a constructed constraint, not a natural one. A drift toward broad international consensus on treaty primacy could change this assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_naturalness_ambiguity, conceptual, 'Whether treaty primacy is a natural principle or a contested reading').

omega_variable(
    performance_vs_binding_force,
    'As theater increases (rising from 0.22 to 0.41), is the constraint maintaining its binding force, or is it being hollowed out — consultations performed while outcomes remain predetermined by state actors?',
    'Empirical audit: do Indigenous nations actually block secession attempts via treaty claims, or do they engage in consultation that states then work around? Track cases where Indigenous consent would have changed the outcome vs. cases where consultation occurred but was non-binding in practice.',
    'If consultations are increasingly performative, the constraint is drifting toward piton territory (maintained theatrically, with declining real power). If Indigenous nations are actually using treaty rights to veto or reshape secession proposals, the constraint remains a binding coordination mechanism. This directly affects whether the measured suppression translates to real extraction or is becoming ceremonial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_binding_force, empirical, 'Whether the constraint maintains binding force as theater increases').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(sece_tr_t0, observed).
narrative_ontology:measurement(sece_tr_t5, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(sece_tr_t5, observed).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(sece_tr_t10, observed).
narrative_ontology:measurement(sece_tr_t15, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(sece_tr_t15, observed).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(sece_tr_t20, observed).
narrative_ontology:measurement(sece_tr_t25, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(sece_tr_t25, observed).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(sece_tr_t30, observed).
narrative_ontology:measurement(sece_tr_t35, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(sece_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(sece_be_t0, observed).
narrative_ontology:measurement(sece_be_t5, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(sece_be_t5, observed).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(sece_be_t10, observed).
narrative_ontology:measurement(sece_be_t15, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(sece_be_t15, observed).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(sece_be_t20, observed).
narrative_ontology:measurement(sece_be_t25, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(sece_be_t25, observed).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(sece_be_t30, observed).
narrative_ontology:measurement(sece_be_t35, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(sece_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sece_su_t0, observed).
narrative_ontology:measurement(sece_su_t5, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(sece_su_t5, observed).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(sece_su_t10, observed).
narrative_ontology:measurement(sece_su_t15, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(sece_su_t15, observed).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(sece_su_t20, observed).
narrative_ontology:measurement(sece_su_t25, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(sece_su_t25, observed).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(sece_su_t30, observed).
narrative_ontology:measurement(sece_su_t35, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(sece_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__treaty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the secession_legitimacy_boundary kernel. The treaty_primacy_reading asserts that pre-confederation Indigenous treaty rights supersede both federal and provincial authority, making them binding gates on secession legitimacy. The constitutional_impossibility_reading asserts that unilateral secession is simply not permitted by constitutional law (independently of treaties); the popular_sovereignty_reading asserts that provincial democratic majorities hold ultimate authority (also independently of treaties); and the grievance_threshold_reading asserts that secession becomes legitimate when federal injustice crosses a threshold (independently of constitutional or treaty text). These are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different enforcement mechanisms. Each reading instantiates the same kernel (what makes secession legitimate) but produces a different constraint. They are linked via network.affects_constraints to show structural influence, not substitution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__treaty_primacy_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
