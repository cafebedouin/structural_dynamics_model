% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132 as Eternal, Immutable Law of Plural Marriage Required for Exaltation
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   This story authors the immutable-commandment reading of the eternal
 *   marriage covenant kernel: D&C 132 recorded and later canonized as a
 *   fixed, unrevisable eternal law that makes plural marriage a requirement
 *   for the highest degree of exaltation. Under this reading, the 1890
 *   Manifesto and subsequent civil compliance are read either as apostasy or
 *   as pure external suppression of a truth that remains binding in eternity
 *   regardless of temporal practice — this reading treats federal pressure as
 *   martyrdom-generating persecution rather than as legitimate grounds for
 *   doctrinal revision, and it recognizes no internal mechanism by which the
 *   commandment could be rightly altered. This is the reading held by
 *   fundamentalist successor groups and by some contemporary critics
 *   reconstructing the doctrine's original claims; it is NOT the reading held
 *   by the mainstream institutional church after 1890 (see the sibling
 *   temporal_accommodation_reading and prophetic_override_reading, authored
 *   as separate constraints).
 *
 * KEY AGENTS:
 *   - senior_male_church_hierarchy: agenda_setter/institutional — administers sealings, enforces doctrinal fixity
 *   - polygamist_patriarchs: beneficiary/powerful — gains household labor, status, guaranteed exaltation path
 *   - plural_wives: payer/powerless — trapped exit, bears the doctrine's material and theological cost
 *   - monogamous_first_wives: payer/powerless — coerced acceptance framed as spiritual requirement
 *   - children_of_plural_families: payer/powerless — inherits contested status across generations
 *   - dissenting_members_facing_excommunication: excluded/moderate — theological objection has no internal forum
 *   - federal_government: excluded/institutional — external pressure treated as illegitimate rather than input
 *   - historians_and_outside_scholars: observer/analytical — sees the documentary record independent of the doctrine's own claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.71).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.8).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132 as Eternal, Immutable Law of Plural Marriage Required for Exaltation").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, '6aa07840-61d9-4e36-88ad-4c414d49a9ef').
narrative_ontology:cs_kernel_codification('6aa07840-61d9-4e36-88ad-4c414d49a9ef', fixed_text).
narrative_ontology:cs_authority_grounding('6aa07840-61d9-4e36-88ad-4c414d49a9ef', lineage).
narrative_ontology:cs_interpretation_layer_present('6aa07840-61d9-4e36-88ad-4c414d49a9ef').
narrative_ontology:cs_reading_relation('6aa07840-61d9-4e36-88ad-4c414d49a9ef', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('6aa07840-61d9-4e36-88ad-4c414d49a9ef', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('6aa07840-61d9-4e36-88ad-4c414d49a9ef', foundational, commandment_textually_self_declared_unrevisable).
narrative_ontology:cs_axiom_status(commandment_textually_self_declared_unrevisable, holdable).
narrative_ontology:cs_axiom_grounding('6aa07840-61d9-4e36-88ad-4c414d49a9ef', commandment_textually_self_declared_unrevisable, deontological).
narrative_ontology:cs_axiom('6aa07840-61d9-4e36-88ad-4c414d49a9ef', foundational, eternal_law_binds_independent_of_civil_authority).
narrative_ontology:cs_axiom_status(eternal_law_binds_independent_of_civil_authority, holdable).
narrative_ontology:cs_axiom_grounding('6aa07840-61d9-4e36-88ad-4c414d49a9ef', eternal_law_binds_independent_of_civil_authority, theological).
narrative_ontology:cs_axiom('6aa07840-61d9-4e36-88ad-4c414d49a9ef', secondary, compliance_with_federal_prohibition_constitutes_apostasy).
narrative_ontology:cs_axiom_status(compliance_with_federal_prohibition_constitutes_apostasy, holdable).
narrative_ontology:cs_axiom_grounding('6aa07840-61d9-4e36-88ad-4c414d49a9ef', compliance_with_federal_prohibition_constitutes_apostasy, deontological).
narrative_ontology:cs_reference_frame('6aa07840-61d9-4e36-88ad-4c414d49a9ef', nauvoo_era_revelatory_authority).
narrative_ontology:cs_drift_state('6aa07840-61d9-4e36-88ad-4c414d49a9ef', post_manifesto_institutional_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('6aa07840-61d9-4e36-88ad-4c414d49a9ef', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, senior_male_church_hierarchy).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, polygamist_patriarchs).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, plural_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, monogamous_first_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, children_of_plural_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, dissenting_members_facing_excommunication).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, abrahamic_covenant_restoration_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, joseph_smith_prophetic_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the sealing ordinances, adjudicates who may enter plural marriage, and enforces the doctrine's textual status as unrevisable revelation. Their authority is constituted by the claim that the revelation is binding and eternal; conceding revisability would concede the mechanism by which they hold priesthood keys. They receive expanded kinship networks, political consolidation, and doctrinal legitimacy from the arrangement.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, senior_male_church_hierarchy, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, senior_male_church_hierarchy, beneficiary).

% Enter additional sealings, gaining household labor, social status within the community, and a theologically guaranteed path to exaltation unavailable to monogamous men. Their standing depends on the doctrine remaining fixed; any softening threatens both their marriages' legitimacy and their eternal reward claim.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, polygamist_patriarchs, beneficiary,
    powerful, biographical, identity_locked, local).

% Are sealed into marriages they frequently did not freely negotiate as equals, sharing a husband's resources and attention among several households. Leaving means social excommunication, loss of children's legitimacy within the covenant community, and eternal damnation under the doctrine's own terms — the theology closes the exit it also depends on them accepting.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_wives, payer,
    powerless, biographical, trapped, local).

% Are told a husband taking additional wives is required for his full exaltation and, by extension, theirs. Resistance is framed as spiritual insufficiency. Economic dependency and community isolation from non-member family foreclose exit; the doctrine converts their objection into evidence of unworthiness.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, monogamous_first_wives, payer,
    powerless, biographical, trapped, local).

% Inherit contested legal status, divided paternal attention and resources across large sibling sets, and later (post-1890) social stigma or prosecution risk. They had no voice in the arrangement's formation and bear its downstream consequences across their lifetimes.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, children_of_plural_families, payer,
    powerless, generational, trapped, local).

% Members and lesser clergy who doubt the doctrine's immutability, or who wish to comply with federal law without abandoning membership, are disciplined or excommunicated. Their theological objection — that a genuinely eternal law cannot rest on one man's private revelation without corroborating scriptural precedent — is not entertained inside the institution that would need to hear it.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissenting_members_facing_excommunication, excluded,
    moderate, biographical, constrained, national).

% Prosecutes plural marriage as a crime and threatens the Church's corporate existence and members' civil rights (Edmunds-Tucker Act seizure of assets). From this reading's own internal logic, federal pressure is persecution of true doctrine, not a legitimate claim on the Church's revision process — the reading treats the state's demand as illegitimate rather than as evidence bearing on doctrine.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% Study the textual history of D&C 132, its documented emergence amid Joseph Smith's own plural relationships, and the doctrine's subsequent institutional handling. They are positioned to compare the immutability claim against the documentary record without a stake in either exaltation or excommunication.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, historians_and_outside_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying theological framework that consolidates the early Church's kinship networks, binds converts into an eternal family structure, and gives male priesthood leadership a settled doctrinal basis for succession and household organization during a period of intense external persecution.
% TRANSFER_FUNCTION: Moves domestic labor, reproductive capacity, inheritance claims, and social status from plural wives and their children toward patriarchs and the institutional hierarchy that authorizes and administers the sealings; moves theological legitimacy and unrevisable authority toward the men who interpret and enforce the revelation's textual status.
% ABSENT_VOICES: Plural wives who did not consent as equals had no forum to contest the doctrine's terms from within; dissenting members who doubted the immutability claim were disciplined rather than heard; the federal government's civil-rights framing was excluded from the doctrine's own internal deliberation entirely, treated as external persecution rather than input.
% DISAPPEARANCE_RATIONALE: If the immutable-commandment reading were abandoned, the doctrinal basis for plural marriages already formed would destabilize, the hierarchy's claim to exclusive, unrevisable revelatory authority on this point would weaken, and the entire kinship, inheritance, and household structure built on the doctrine's eternal status would require renegotiation — which is precisely what happened, contested, after 1890.
% FOUNDING_PROBLEM: Joseph Smith's own polygamous relationships needed theological legitimation, and the emerging Church needed a doctrine that could bind a persecuted, geographically scattered community into durable kinship and succession structures under conditions of hostility from outside authorities.
% FOUNDING_PROBLEM_CORROBORATION: Documentary historians outside the Church's own faith-promoting historiography (including non-LDS scholars examining the Nauvoo-era record and the timing of D&C 132's dictation relative to Smith's already-existing plural relationships) attest the founding problem was retroactive legitimation rather than prospective eternal law; the Church's own later Manifesto (1890) and post-Manifesto discipline of continuing practitioners is itself an internal acknowledgment, from within the tradition, that the doctrine's practical operation had become untenable — though the immutable-commandment reading itself denies this and treats the underlying problem as still live in eternity even where practice was suspended.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71 at interval end) because the doctrine converts household labor, reproductive capacity, and inheritance rights into a one-directional flow toward patriarchs and the administering hierarchy, under a coercive theological penalty (damnation) for noncompliance or dissent. Suppression is authored higher still (0.8) because this reading's persistence depends on actively foreclosing revision — dissenters are excommunicated, wives who resist are framed as unworthy, and the doctrine's own textual claim to immutability is the suppression mechanism, not incidental to it. Theater is low-to-moderate (0.28) because the coordination function (kinship consolidation, succession clarity, resistance to persecution) is genuinely operative for the hierarchy even as extraction dominates for the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Senior hierarchy and polygamist patriarchs are structural beneficiaries: they collect status, labor, and theological legitimacy, and their exit is identity-locked because their authority and eternal-reward claims are constituted by the doctrine's fixity — conceding revisability collapses their position, so despite high nominal power their d is not fully beneficiary-end; the identity lock keeps them tethered rather than freely exiting even as they profit. Plural wives, first wives, and children are targets: trapped exit options, powerless structural position, and a theology that converts resistance into evidence of unworthiness push their d toward the full-target end. Dissenting members occupy a constrained middle: moderate power, but institutional discipline forecloses the exit that would let them act on their doubt without cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem (legitimating Joseph Smith's existing plural relationships and consolidating a persecuted community) is authored as dead — the community-consolidation need this doctrine served no longer obtains in a world without frontier persecution and church-state civil war — while the reading itself insists the underlying eternal principle remains live regardless of circumstance. That status/verdict mismatch (dead founding problem, world_rearranges disappearance verdict) is exactly the signal the mandatrophy detection exists to catch: an arrangement whose stated justification has expired while its structural machinery (sealing authority, doctrinal fixity, excommunication discipline) persists and continues to bind real people's exit options.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_evidence,
    'What textual or historical evidence would distinguish the immutable-commandment reading from the prophetic-override and temporal-accommodation readings as the doctrinally correct account of D&C 132''s status?',
    'Comparative analysis of the revelation''s own internal language (''this is a new and everlasting covenant, and if ye abide not that covenant, then are ye damned'') against later institutional statements (1890 Manifesto, 1904 Second Manifesto) and against the doctrine of continuing revelation as institutionally practiced elsewhere (e.g., 1978 priesthood revelation reversing prior racial restriction) to assess whether the Church''s own precedent treats commandments as revisable.',
    'If the Church''s own later practice (e.g., 1978) demonstrates that revelations understood as eternal have in fact been superseded, that undermines this reading''s foreclosure claim and strengthens the prophetic_override_reading; if D&C 132''s text is genuinely singular in its self-declared permanence relative to other revelations, that strengthens this reading''s structural distinctiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Whether internal doctrinal precedent supports treating this specific commandment as uniquely unrevisable versus revisable like other revelations.').

omega_variable(
    beneficiary_capture_of_immutability_claim,
    'Is the immutability claim itself best explained as genuine theological conviction, or as a claim whose persistence is explained by the material and status benefits it delivered to the men positioned to administer and practice plural marriage?',
    'Compare the timing and content of the doctrine''s articulation against the biographical circumstances of its promulgator and earliest practitioners; examine whether comparable persecution-era religious movements produced similarly extractive kinship doctrines without an immutability claim, isolating whether immutability specifically tracks beneficiary interest.',
    'If immutability tracks beneficiary interest closely, this reading functions as a false-summit-style legitimation for extraction dressed as eternal law; if it tracks independent theological reasoning uncorrelated with beneficiary status, the coordination function is more genuine than the extraction framing suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_immutability_claim, conceptual, 'Whether the doctrine''s claimed permanence is explained by genuine conviction or by the interests of those who benefit from its fixity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by plural wives and dissenting members structural (excommunication, economic dependency, legal exposure) or internalized (belief that resistance itself constitutes spiritual unworthiness, absorbed through socialization inside the community)?',
    'Compare post-exit trajectories of women who left plural households or the faith entirely: if reported guilt, self-doubt, and continued deference to the doctrine''s moral framework persist well after structural barriers (financial dependency, social ties) are removed, that indicates a substantial internalized component.',
    'If largely internalized, the effective suppression carried by targets is higher than the structural exit-options atom alone suggests, since the doctrine''s psychological hold outlives formal membership or the marriage itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression among women bound by the doctrine''s terms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(eter_tr_t30, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(eter_tr_t45, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement(eter_tr_t60, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(eter_be_t30, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(eter_be_t45, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 45, 0.7).
narrative_ontology:measurement(eter_be_t60, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 60, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(eter_su_t30, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(eter_su_t45, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 45, 0.8).
narrative_ontology:measurement(eter_su_t60, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 60, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__immutable_commandment_reading, 0.08).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story, prophetic_override_reading, and temporal_accommodation_reading are three readings of the single eternal_marriage_covenant kernel (D&C 132's status). Each reading authors a distinct ε: this reading (immutable_commandment) authors high, undamped extraction because it treats the commandment as presently binding and unrevisable, generating an ongoing coercive claim on adherents; temporal_accommodation_reading would author lower present-tense extraction because it treats practice as suspended even while preserving the doctrine's eternal validity in principle; prophetic_override_reading would author the lowest extraction of the three because it treats the commandment as one among many revisable revelations, fully superseded by subsequent prophetic action with no live present claim on anyone. The three are not the same constraint measured three ways — they make incompatible claims about whether the commandment is presently operative, and each should be read, classified, and evaluated as its own file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
