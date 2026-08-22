% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra Reformist Contextual Reading: Ethical Core Separable from Caste Prescriptions
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The reformist contextual reading of Dharmasastra attempts to preserve the
 *   texts' normative authority over ethical conduct (dharma as righteous
 *   behavior, duty, justice) while rejecting the strict, literal enforcement
 *   of varna-based social hierarchy and caste prescriptions as time-bound
 *   rather than eternal. This reading emerged as institutional response to
 *   colonial critique, nationalist independence movements, and internal
 *   reform activism in 19th–20th century South Asia. It claims the texts'
 *   core ethical principles remain universally valid and binding, but
 *   reinterprets varna as representing spiritual stages of development rather
 *   than hereditary enforcement categories. Lower-varna practitioners, women,
 *   and contemporary advocates for substantive equality bear the structural
 *   cost: they must navigate both the residual enforcement of hierarchy (in
 *   ritual contexts, marriage rules, occupational expectations) and the
 *   rhetorical denial that the hierarchy has coercive force at all (reframed
 *   as 'spiritual difference' rather than legal subordination). The reformist
 *   reading benefits brahminical institutional authority—which retains
 *   control over textual interpretation and the authority to declare which
 *   prescriptions are time-bound—and reformist scholars whose professional
 *   authority depends on maintaining both textual reverence and social-reform
 *   credibility.
 *
 * KEY AGENTS:
 *   - brahminical_institutional_authority: maintains interpretive authority over canonical texts; benefits from preservation of textual reverence; defends hierarchy in softened form
 *   - reformist_scholars: produce the reinterpreted readings; benefit from scholarly authority and institutional positions; mediate between traditional authority and reform pressure
 *   - lower_varna_practitioners: experience enforcement of hierarchy alongside denial that hierarchy is being enforced; constrained by ritual expectations, marriage rules, occupational restrictions
 *   - womens_advocacy_movements: challenge prescriptions about womens_role_dharmasastra as time-bound, but encounter the same ambiguity: are womens restrictions ethical principles or contextual prescriptions?
 *   - orthodox_literalist_authority: opposes the reading; sees it as delegitimizing the texts; argues for literal observance of all prescriptions
 *   - abolitionist_scholars: argue the reading cannot succeed; sees it as performing institutional preservation under reform rhetoric; calls for complete rejection of the framework
 *   - state_authority: enforces legal equality while texts remain culturally authoritative; encounters the constraint indirectly through marriage law, education policy, ritual autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.52).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.48).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.52).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra Reformist Contextual Reading: Ethical Core Separable from Caste Prescriptions").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'bd980c3a-f772-4a74-8756-060f21d15d07').
narrative_ontology:cs_kernel_codification('bd980c3a-f772-4a74-8756-060f21d15d07', fixed_text).
narrative_ontology:cs_authority_grounding('bd980c3a-f772-4a74-8756-060f21d15d07', lineage).
narrative_ontology:cs_interpretation_layer_present('bd980c3a-f772-4a74-8756-060f21d15d07').
narrative_ontology:cs_reading_relation('bd980c3a-f772-4a74-8756-060f21d15d07', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('bd980c3a-f772-4a74-8756-060f21d15d07', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('bd980c3a-f772-4a74-8756-060f21d15d07', foundational, ethical_core_eternally_binding).
narrative_ontology:cs_axiom_status(ethical_core_eternally_binding, holdable).
narrative_ontology:cs_axiom_grounding('bd980c3a-f772-4a74-8756-060f21d15d07', ethical_core_eternally_binding, deontological).
narrative_ontology:cs_axiom('bd980c3a-f772-4a74-8756-060f21d15d07', foundational, hierarchy_prescriptions_contextual_not_eternal).
narrative_ontology:cs_axiom_status(hierarchy_prescriptions_contextual_not_eternal, holdable).
narrative_ontology:cs_axiom_grounding('bd980c3a-f772-4a74-8756-060f21d15d07', hierarchy_prescriptions_contextual_not_eternal, empirically_contingent).
narrative_ontology:cs_reference_frame('bd980c3a-f772-4a74-8756-060f21d15d07', ethical_core_separable_from_hierarchy).
narrative_ontology:cs_drift_state('bd980c3a-f772-4a74-8756-060f21d15d07', contemporary_legal_equality_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bd980c3a-f772-4a74-8756-060f21d15d07', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, brahminical_institutional_authority).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_scholars).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_varna_practitioners).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, womens_advocacy_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, reformist_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls interpretive authority over Dharmasastra texts through brahminical institutional structures (temples, learned councils, educational lineages). Declares which prescriptions are eternal ethical principles and which are time-bound contextual norms. Benefits from preservation of textual reverence and their authority to interpret. Can shift between literalist and reformist framings depending on institutional pressure and social conditions. Maintains ritual authority, influence over marriage law and social norms, and scholarly prestige through the interpretive monopoly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, brahminical_institutional_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% Produce reinterpreted readings of Dharmasastra that preserve textual authority while rejecting caste enforcement. Benefit from scholarly prestige, institutional positions in universities and reform organizations, and authority to shape public discourse on the texts' relevance. Pay by defending an unstable middle position attacked from both traditionalists (who see betrayal of the texts) and abolitionists (who see enabling of hierarchy). Their professional survival depends on maintaining both textual reverence and reform credibility, creating constant pressure and institutional vulnerability.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_scholars, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, reformist_scholars, payer).

% Experience the constraints of hierarchy—ritual restrictions, occupational expectations, marriage-rule limitations—in daily life. Encounter the reformist reading's claim that these restrictions are no longer binding, but find that social enforcement persists (families still expect compliance, ritual contexts enforce rules, occupational segregation continues). Are trapped in the hierarchy through cultural embedding and economic interdependence. The reinterpreted reading provides rhetorical relief but not material change in enforcement. Cannot easily exit the tradition without bearing high cultural costs.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_varna_practitioners, payer,
    powerless, biographical, trapped, regional).

% Challenge dharmasastra prescriptions about women's role (duty to husband, restricted education, marriage rules, ritual participation limits) as time-bound rather than eternally binding. Face the same reinterpretation problem as lower-varna practitioners: the reformist reading claims women's restrictions are contextual, but ritual contexts and family structures continue to enforce them. Have some organized power through advocacy networks and legal advocacy, but encounter structural resistance from both brahminical authority (which wants to preserve textual reverence) and from women practitioners who accept the reinterpreted hierarchy as spiritually legitimate. Exit is constrained: can advocate for change within the framework or reject the framework, both costly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, womens_advocacy_movements, payer,
    organized, generational, constrained, regional).

% Oppose the reformist reading as betrayal of the texts' literal authority. Argue that all dharmasastra prescriptions, including varna hierarchy, are eternally binding and should be enforced literally. Have institutional presence in conservative temples and traditional educational lineages but have less influence in urban contexts and state institutions. Are excluded from the reformist reading's decision-making about which prescriptions are time-bound; their voice is present in public debate but not in institutional interpretation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalist_authorities, excluded,
    institutional, generational, constrained, regional).

% Argue the reformist reading cannot succeed because the ethical core and the hierarchy are structurally inseparable; that the attempt to preserve textual authority while discarding caste is performative preservation of oppressive hierarchy under reform rhetoric. Advocate complete rejection of the Dharmasastra framework. Have scholarly presence and growing social movement influence, especially among younger generations and urban practitioners. Are excluded from brahminical authority structures but have exit option (can leave the tradition) that lower-varna practitioners lack. Attack both the reformist reading and the literalist reading as insufficiently radical.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_scholars, excluded,
    organized, generational, mobile, regional).

% Enforces legal equality and constitutional protections against caste discrimination, but does not directly enforce the Dharmasastra reading one way or another. Encounters the constraint indirectly through personal law (marriage, inheritance, family structure remain partly governed by religious tradition), education policy (texts are studied in culture curricula), and ritual autonomy (permits religious communities to maintain their practices). Creates structural pressure on the reading by enforcing legal equality in secular domains, which makes the reinterpreted hierarchy increasingly difficult to sustain through explicit social enforcement. Takes no direct position on which reading of Dharmasastra is correct but creates conditions that make the reformist reading's non-enforced hierarchy increasingly unstable.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, state_legal_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__reformist_contextual, brahminical_institutional_authority).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__reformist_contextual, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for understanding ethical conduct, social roles, justice, and personal duty that is grounded in authoritative texts and traditionally transmitted interpretation. Solves the coordination problem of: how should people conduct themselves ethically, what are their duties, and how should social relationships be structured? The reading coordinates on the principle that dharma (righteous conduct) is an eternally binding ethical framework.
% TRANSFER_FUNCTION: Moves authority and interpretive power from those who follow the texts literally or reject them entirely, to those (brahminical institutional authority and reformist scholars) who control reinterpretation. Transfers social compliance from literal enforcement of hierarchy to internalized acceptance of reinterpreted hierarchy (varna as spiritual difference rather than legal enforcement). Transfers the labor of reconciling traditional authority with modern equality from institutions to individual practitioners (who must internally hold both the reinterpreted hierarchy and legal equality as compatible).
% ABSENT_VOICES: Fully marginalized lower-caste movements that have historically rejected the framework entirely; pre-modern dissenting traditions within Dharmasastra interpretation (the Carvaka, Buddhist critiques of caste); secular practitioners who would question whether any textual framework should govern social relations. These voices were historically excluded through institutional control of textual interpretation and education. Contemporary abolitionist scholars are partially present but excluded from interpretive authority.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight and only literalist and abolitionist readings remained, South Asian institutions would rearrange significantly: either caste hierarchy would need to be defended explicitly as eternally binding (literalist dominance, generating massive social conflict), or the entire Dharmasastra framework would be abandoned (abolitionist dominance, destroying institutional continuity for brahminical authority and requiring alternative frameworks for dharma/ethics in Hindu practice). The reading's function is to allow textual authority and social equality to coexist rhetorically without requiring institutional abandonment of either. Its disappearance would force a choice.
% FOUNDING_PROBLEM: How can the Dharmasastra texts retain normative authority and legitimacy for modern practitioners and institutions while the literal caste hierarchy it prescribes is no longer socially or legally acceptable?
% FOUNDING_PROBLEM_CORROBORATION: Brahminical authority and reformist scholars attest the founding problem is live and ongoing—they continue to work on the reinterpretation project. Lower-varna practitioners and women's advocates attest the founding problem is increasingly dead—they have moved beyond the question of how to preserve textual authority and toward whether the texts deserve authority at all. Independent scholars and social historians document that the reinterpretation project emerged as institutional response to colonial critique and reform activism, suggesting the problem was constructed rather than discovered. Legal theorists attest that the problem persists only for those invested in maintaining textual reverence; secular legal frameworks do not face this problem because they do not require textual authority.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the constraint operates through reinterpreted rather than explicit hierarchy: the extraction is present (lower-varna practitioners still experience ritual restrictions, marriage-rule constraints, occupational expectations) but is clothed in language of spiritual development rather than coercive enforcement. This mismatch between structural effect and rhetorical framing is what moderate extractiveness captures. Suppression is comparable (0.48) because resistance is partly structural (lower-caste movements, women's advocacy, secular-law advocates push back explicitly) and partly internalized (many practitioners accept the spiritual-stage framing and comply without experiencing it as coercive). Theater is rising (from 0.22 to 0.41): the measurement series reflects increased reliance on performative reinterpretation—the core ethical claims remain verbally authoritative, but more enforcement activity defends the reinterpreted hierarchy itself rather than demonstrating its ethical necessity. The measurement series tracks the temporal trajectory from strict traditionalist enforcement (high suppression, lower theater, higher extractiveness) through successful reform movement adoption (lower suppression, higher theater, reduced extractiveness) toward stabilization of the reformist reading. The grid is shared: every metric is authored at every time point (0, 4, 8, 12, 16, 20), avoiding temporal misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (brahminical authority) experiences this reading as successful preservation of textual authority against reform pressure—they have maintained control over interpretation and authority while appearing to address reform concerns. Lower-varna practitioners experience it as constrained coercion wearing a new rhetorical mask: the restrictions persist but are now claimed to be 'freely chosen' spiritual differences rather than binding duties. Reformist scholars experience it as a difficult middle position: they genuinely believe in the ethical core's separability and in preserving the texts' authority, but they encounter continuous pressure from both directions (traditionalists accuse them of betrayal; abolitionists accuse them of enabling ongoing hierarchy). The engine computes these divergent classifications from the structural data—the claimed type (tangled_rope: both coordination and extraction) reflects the scheduler's own understanding that the reading coordinates on the ethical principles while extracting through the reinterpreted hierarchy, but different seats experience it differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical institutional authority occupies the agenda-setting seat: they control textual interpretation, declare which prescriptions are time-bound and which are eternal ethical principles, and maintain their authority through that interpretive monopoly. Their directionality is low (beneficiary end): they collect the authority rent from preserving textual reverence. Reformist scholars sit near symmetric or slightly beneficiary-leaning: they benefit from authority and institutional positions, but bear real costs in defending a position that is attacked from both sides (traditionalists say they betray the texts; abolitionists say they enable hierarchy). Lower-varna practitioners and women's advocates are the structural targets: they experience constraints (hierarchy, restriction) while the language shifts to deny the constraints are binding. Their exit options are constrained—leaving the religious tradition is culturally costly, and the reformist reinterpretation is presented as more progressive than the traditional reading, creating a false choice between 'accept the reinterpreted hierarchy' and 'abandon your tradition entirely.' State authority sits as observer: it cannot directly alter the reading but creates structural pressure through legal equality enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem for the reformist reading is: 'How can we preserve Dharmasastra's ethical authority and the texts' legitimacy within modern consciousness while abandoning the caste hierarchy's coercive force?' This is a live problem in the reading's own reference frame—brahminical authority and reformist scholars actively work to maintain textual reverence while appearing to abandon caste enforcement. However, for lower-varna practitioners and women's advocates, the founding problem is increasingly dead: they have moved beyond the question of how to preserve the texts' authority and toward the question of whether the texts retain any legitimate authority at all. This (live vs. dead founding problem) mismatch is the mandatrophy signature. The reformist reading attempts to resolve the contradiction by claiming the ethical core remains eternally binding while the hierarchy is merely time-bound—but this resolution works only if the interpreter has the authority to declare which prescriptions are eternal and which are contextual. That interpretive authority IS what lower-varna practitioners and women's advocates contest. The constraint persists because brahminical authority and reformist scholars continue to assert and defend it, and because many practitioners internalize the reinterpreted hierarchy as legitimate. But the founding problem (preserving textual authority while abandoning caste) has become zombie-like: the problem exists for the benefiting parties, but the victims have largely abandoned the problem-space itself, asking instead whether the framework deserves authority at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the ethical core (dharma as righteous conduct) genuinely separable from the hierarchical prescriptions, or does the hierarchy represent the primary substantive content with ethics as superstructure?',
    'Genealogical analysis of textual layers: which claims originated when, which served which institutional interests; cross-cultural comparison with other hierarchical religious systems'' reform strategies.',
    'If core and hierarchy are structurally inseparable, this reading collapses into the abolitionist_rejection (no legitimate core survives); if genuinely separable, the reading can preserve institutional authority while discarding caste enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the ethical core is separable from the caste system or intrinsically bound to it.').

omega_variable(
    victim_set_ambiguity,
    'Does reinterpreting varna as spiritual stages rather than hereditary enforcement meaningfully reduce extraction from lower-varna practitioners, or does it merely ritualize the same hierarchy under a different conceptual frame?',
    'Ethnographic comparison: do practitioners in contexts adopting this reading report reduced coercive enforcement, increased social mobility, altered access to resources and ritual authority? Or do the same patterns persist with reinterpreted language?',
    'If extraction persists under reframing, the measured extractiveness (0.52) is understated and the constraint should reclassify toward snare; if enforcement genuinely softens, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_ambiguity, empirical, 'Whether spiritual reinterpretation materially reduces enforcement or merely relabels it.').

omega_variable(
    authority_preservation_intent,
    'Is the reformist reading''s effort to preserve Dharmasastra authority while discarding caste prescriptions stable, or does it face inherent erosion pressures from both traditionalists and abolitionists?',
    'Historical trajectory analysis: does this reading stabilize as institutional doctrine, or is it a temporary position eroding toward one of the sibling readings over the next generation?',
    'If stable, the reading''s classification is durable; if eroding, the reading is itself staging a transition state (scaffolding function), not a steady-state constraint classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_preservation_intent, empirical, 'Whether the reformist middle position is sustainable or transitional.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.48) structural (legal/institutional barriers to dissent) or internalized (practitioners'' own belief that the reinterpreted hierarchy is still legitimately binding)?',
    'Post-reform-adoption trajectory: in contexts where reformist reinterpretation becomes policy, does suppression decrease after barrier removal, or do practitioners'' internalized compliance persist?',
    'If internalized, the effective suppression persists after institutional barriers fall, and the constraint''s actual reach is higher than the measured 0.48; if structural, removal of institutional enforcement would materially reduce compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized belief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.22).
narrative_ontology:measurement(dhar_tr_t4, dharmasastra_corpus__reformist_contextual, theater_ratio, 4, 0.28).
narrative_ontology:measurement(dhar_tr_t8, dharmasastra_corpus__reformist_contextual, theater_ratio, 8, 0.33).
narrative_ontology:measurement(dhar_tr_t12, dharmasastra_corpus__reformist_contextual, theater_ratio, 12, 0.37).
narrative_ontology:measurement(dhar_tr_t16, dharmasastra_corpus__reformist_contextual, theater_ratio, 16, 0.4).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__reformist_contextual, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(dhar_be_t4, dharmasastra_corpus__reformist_contextual, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(dhar_be_t8, dharmasastra_corpus__reformist_contextual, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(dhar_be_t12, dharmasastra_corpus__reformist_contextual, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(dhar_be_t16, dharmasastra_corpus__reformist_contextual, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__reformist_contextual, base_extractiveness, 20, 0.51).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(dhar_su_t4, dharmasastra_corpus__reformist_contextual, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(dhar_su_t8, dharmasastra_corpus__reformist_contextual, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(dhar_su_t12, dharmasastra_corpus__reformist_contextual, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(dhar_su_t16, dharmasastra_corpus__reformist_contextual, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__reformist_contextual, suppression_requirement, 20, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__reformist_contextual, 0.12).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% The Dharmasastra corpus kernel generates three distinct constraint stories corresponding to three live readings of the texts' normative authority. The reformist_contextual reading (this story) claims the ethical core is separable from hierarchical prescriptions; the orthodox_literalist reading claims all prescriptions are eternally binding; the abolitionist_rejection reading claims the entire framework is fundamentally oppressive. Each reading produces a different extractiveness profile, different victim set, and different classification. Network edges link the stories as siblings in the same kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
