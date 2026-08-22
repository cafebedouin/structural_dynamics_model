% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Constitutional Secularism — Principled Intervention Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the 'principled intervention' reading of the
 *   constitutional secularism kernel: the state is constitutionally
 *   authorized to intervene in religious affairs specifically to advance
 *   social reform and protect weaker sections within religious communities,
 *   distinguishing 'essential' religious practice (protected) from
 *   'secular/social' practice attached to religion (reformable). This is a
 *   distinct constitutional claim from strict neutrality (equal distance, no
 *   interference) and from the reformist reading (an affirmative state duty
 *   overriding religious autonomy wholesale). Under this reading,
 *   intervention is bounded and justificatory — it requires an articulated
 *   reform rationale and a doctrinal essentiality test — but the boundary is
 *   state-administered, which is exactly where the coordination function
 *   (protecting the internally powerless) and the extraction risk
 *   (majoritarian or selective targeting of weaker communities) become
 *   inseparable in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.52).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.48).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Constitutional Secularism — Principled Intervention Reading").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '9362681d-78d9-4d92-9f68-a4aae07894b6').
narrative_ontology:cs_kernel_codification('9362681d-78d9-4d92-9f68-a4aae07894b6', formalized).
narrative_ontology:cs_authority_grounding('9362681d-78d9-4d92-9f68-a4aae07894b6', lineage).
narrative_ontology:cs_interpretation_layer_present('9362681d-78d9-4d92-9f68-a4aae07894b6').
narrative_ontology:cs_reading_relation('9362681d-78d9-4d92-9f68-a4aae07894b6', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('9362681d-78d9-4d92-9f68-a4aae07894b6', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('9362681d-78d9-4d92-9f68-a4aae07894b6', foundational, reform_justification_permits_bounded_intervention).
narrative_ontology:cs_axiom_status(reform_justification_permits_bounded_intervention, holdable).
narrative_ontology:cs_axiom_grounding('9362681d-78d9-4d92-9f68-a4aae07894b6', reform_justification_permits_bounded_intervention, conventional).
narrative_ontology:cs_axiom('9362681d-78d9-4d92-9f68-a4aae07894b6', foundational, essential_practice_test_limits_state_reach).
narrative_ontology:cs_axiom_status(essential_practice_test_limits_state_reach, holdable).
narrative_ontology:cs_axiom_grounding('9362681d-78d9-4d92-9f68-a4aae07894b6', essential_practice_test_limits_state_reach, conventional).
narrative_ontology:cs_reference_frame('9362681d-78d9-4d92-9f68-a4aae07894b6', constitutional_founding_reform_mandate).
narrative_ontology:cs_drift_state('9362681d-78d9-4d92-9f68-a4aae07894b6', contemporary_majoritarian_politics_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9362681d-78d9-4d92-9f68-a4aae07894b6', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, reform_minded_state_institutions).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, intra_religious_reform_movements).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, socially_vulnerable_coreligionists).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, orthodox_religious_authorities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, minority_religious_communities_targeted_for_reform).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_autonomy_claimants).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, essential_practices_doctrine).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, constitutional_morality_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures and courts that identify specific religious practices as socially harmful (untouchability, discriminatory temple entry, certain personal-law provisions) and enact or uphold statutes overriding religious autonomy claims to reach them. They set the doctrinal test (what counts as 'essential' to a religion, what counts as legitimate 'social welfare or reform') and administer enforcement through courts, boards, and administrative takeover of religious institutions.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, reform_minded_state_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Lower-caste worshippers, women, and other marginalized members of a religious community who are denied temple entry, priesthood access, or equal ritual standing by internal religious hierarchy. State intervention is often their only practical route to a remedy, since they cannot exit the community without severe social cost and cannot win the internal doctrinal argument on their own.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, socially_vulnerable_coreligionists, beneficiary,
    powerless, biographical, trapped, national).

% Reformist factions within a religious tradition who lack the numbers or institutional standing to win internal doctrinal disputes but can invoke state power to force change. They gain leverage disproportionate to their internal community standing by aligning with state reform objectives.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, intra_religious_reform_movements, beneficiary,
    moderate, generational, constrained, national).

% Priests, boards, and traditional custodians who administer religious institutions according to inherited doctrine. They face state-imposed redefinition of which of their practices count as 'essential' (and thus protected) versus 'secular/social' (and thus subject to override). They cannot exit the jurisdiction of the state's essential-practices test without abandoning legal recognition of the institution itself; litigation is their main recourse and frequently fails against the reform framing.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, orthodox_religious_authorities, payer,
    organized, civilizational, constrained, national).

% Smaller or politically weaker religious communities whose practices are more readily labeled 'social' rather than 'religious' by a majoritarian state apparatus, exposing them to intervention that comparably-situated majority-community practices escape. Their limited political weight means the reform lens is applied to them more readily and contested less successfully.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, minority_religious_communities_targeted_for_reform, payer,
    moderate, generational, constrained, national).

% Individuals and institutions asserting a sincere claim that a challenged practice is doctrinally essential, not merely social custom. They must litigate against a state-administered essentiality test that they do not control and that shifts with judicial composition and political climate.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_autonomy_claimants, payer,
    moderate, biographical, constrained, national).

% Adjudicate the boundary between protected religious essence and reformable social practice case by case, articulating and revising the essential-practices doctrine. Their rulings both constrain and legitimize the intervention power, making them simultaneously referees and co-architects of the constraint.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, constitutional_courts, agenda_setter).

% Political actors who can selectively invoke the reform rationale to target minority religious practices for electoral advantage while leaving majority-community analogues untouched. Not formally part of the doctrinal test, but able to shape which practices get labeled targets for reform through legislative agenda-setting and public pressure on courts.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, majoritarian_political_actors, excluded,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__principled_intervention_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__principled_intervention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional mechanism by which internally powerless members of a religious community — who cannot win an argument inside the community's own doctrinal structure — can obtain relief from the state against practices that harm them, without requiring the community's own hierarchy to consent to the change.
% TRANSFER_FUNCTION: Moves interpretive and administrative authority over what counts as 'essential' religious doctrine from religious hierarchies to state institutions (legislatures and courts), and moves practical relief (temple access, altered personal-law treatment, institutional reorganization) from orthodox custodians to previously excluded community members.
% ABSENT_VOICES: Minority religious communities without political weight rarely get a seat in defining which of their practices will be tested for 'essentiality' — the criteria are set primarily with reference to majority-community jurisprudence and then applied outward. Ordinary lay adherents who are neither reformers nor orthodox leadership (the largest group) are rarely heard directly in litigation framed as hierarchy-versus-state.
% DISAPPEARANCE_RATIONALE: If the state lost the constitutional authority to intervene in religious affairs for reform purposes, temple-entry mandates, anti-discrimination rulings inside religious institutions, and reformist statutes overriding religious personal law would lose their legal foundation; socially vulnerable coreligionists would lose their primary lever against internal hierarchy, and orthodox authorities would regain full control over practice definition. Political actors would lose a tool currently available for selective, asymmetric intervention.
% FOUNDING_PROBLEM: Deeply entrenched practices within some religious communities (untouchability, temple-entry exclusion, discriminatory treatment of women) inflicted serious material and dignitary harm on members who had no realistic internal path to reform, and a strict non-interference rule would have frozen those harms in place indefinitely.
% FOUNDING_PROBLEM_CORROBORATION: Affected community members and independent civil-rights litigators outside the state apparatus attest the underlying harms (caste-based exclusion, gender discrimination in religious institutions) remain live in specific communities, corroborating that the founding problem persists in some domains. Comparative constitutional scholars and minority-rights organizations, however — voices outside the reform-beneficiary set — attest that the doctrine's application has become asymmetric, applied more readily against politically weaker minority communities than against majority-community analogues, suggesting the tool has partly drifted from its founding justification toward selective political use.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) reflects a real, non-trivial cost imposed on orthodox authorities and especially on politically weak minority communities whose practices are more readily classified as 'social' rather than 'essential' — the same doctrinal test applied with different practical stringency depending on the political weight of the community it is applied to. Suppression (0.48) reflects that the essentiality test itself is state-administered and revisable by courts without the tested community's consent — a real constraint on religious autonomy, though bounded by judicial process rather than raw administrative fiat. Theater is low-moderate (0.28): the doctrine does real reformist work (temple-entry orders, anti-discrimination rulings have had measurable effects) but a growing share of its application over time is asymmetric and politically selective rather than harm-driven, which is what the theater trend captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Socially vulnerable coreligionists and intra-religious reformers are structural beneficiaries — the whole point of this reading is to give them a lever they lack internally, so their directionality sits near the beneficiary end despite most having low nominal power (the constraint inverts the power gradient in their favor by design). Orthodox authorities and religious autonomy claimants are structural targets — they lose interpretive control they previously held, so directionality sits near the target end even though their nominal power (organized, institutional standing) is substantial. Minority communities targeted for reform sit doubly disadvantaged: they may contain genuinely powerless victims of internal practice AND face a comparatively harsher application of the reform test than majority-community analogues — a directionality effect that a flat beneficiary/victim read would understate without the majoritarian-capture omega below.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (entrenched intra-community harms with no internal remedy) remains live in specific, identifiable cases, which is why this cannot simply be read as an obsolete mandate — the world would genuinely rearrange (vulnerable coreligionists would lose a working remedy) if the intervention power vanished. But the corroboration split — beneficiary communities and civil-rights litigators still see live harms, while comparative scholars and minority-rights groups outside the reform-beneficiary set report the doctrine drifting toward selective political application — is the signature of partial mandatrophy: the coordination function has not fully atrophied, but an extraction layer (majoritarian selective enforcement) has grown alongside it rather than replacing it. That is the tangled_rope signature exactly: coordination and extraction sharing the same structure, not one masquerading as the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    essentiality_test_neutrality,
    'Is the state''s essential-practices test applied with equal doctrinal rigor across majority and minority religious communities, or does it function with a majoritarian bias that exposes politically weaker communities to intervention more readily than comparably-situated majority practices?',
    'Comparative empirical analysis of essentiality rulings across religious communities of differing political weight, controlling for the severity and nature of the underlying practice, to detect systematic asymmetry in outcomes.',
    'If bias is confirmed, the doctrine''s operation for minority communities functions closer to a snare (majoritarian extraction dressed as reform) even while the same doctrine functions closer to a rope for majority-community internal reform movements — supporting a within-reading directionality split rather than a uniform tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essentiality_test_neutrality, empirical, 'Whether the essentiality test is applied with majoritarian asymmetry.').

omega_variable(
    reading_boundary_stability,
    'Where exactly does the principled-intervention reading''s boundary sit relative to the reformist reading — is ''reform objective'' a genuinely limiting criterion, or does it collapse into the reformist reading''s categorical override once courts defer heavily to legislative reform framing?',
    'Doctrinal history tracing whether courts have ever declined to find a reform justification once legislative intent was clearly reformist, versus cases where courts held religious autonomy to prevail despite an asserted reform rationale.',
    'If courts never decline the reform framing once asserted, the principled-intervention reading is not structurally distinct from the reformist reading in practice, though the two remain distinct as constitutional doctrines — this would be a conceptual, not empirical, collapse and would matter for how sharply the two readings'' constraints should be kept ε-distinct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_stability, conceptual, 'Whether principled intervention functions as a genuine limiting boundary or drifts toward the reformist reading''s categorical override in practice.').

omega_variable(
    reform_beneficiary_capture,
    'Do the socially vulnerable coreligionists who are the doctrine''s intended beneficiaries actually receive durable relief, or does the intervention primarily transfer interpretive authority to state institutions and reformist elites without durable improvement in the beneficiaries'' material or social position?',
    'Longitudinal tracking of specific interventions (temple-entry orders, personal-law reforms) for durability of on-the-ground compliance and beneficiary outcomes, versus formal legal victories that are not enforced in practice.',
    'If relief is largely formal without durable enforcement, the coordination function is substantially theatrical and the constraint''s true operation is closer to enforcement-mechanism legitimation for state authority expansion than actual protection of weaker sections.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_beneficiary_capture, empirical, 'Whether intervention produces durable beneficiary relief or primarily formal/symbolic outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t12, constitutional_secularism__principled_intervention_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(cons_tr_t24, constitutional_secularism__principled_intervention_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(cons_tr_t36, constitutional_secularism__principled_intervention_reading, theater_ratio, 36, 0.22).
narrative_ontology:measurement(cons_tr_t48, constitutional_secularism__principled_intervention_reading, theater_ratio, 48, 0.25).
narrative_ontology:measurement(cons_tr_t60, constitutional_secularism__principled_intervention_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement(cons_tr_t70, constitutional_secularism__principled_intervention_reading, theater_ratio, 70, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cons_be_t12, constitutional_secularism__principled_intervention_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(cons_be_t24, constitutional_secularism__principled_intervention_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(cons_be_t36, constitutional_secularism__principled_intervention_reading, base_extractiveness, 36, 0.45).
narrative_ontology:measurement(cons_be_t48, constitutional_secularism__principled_intervention_reading, base_extractiveness, 48, 0.48).
narrative_ontology:measurement(cons_be_t60, constitutional_secularism__principled_intervention_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(cons_be_t70, constitutional_secularism__principled_intervention_reading, base_extractiveness, 70, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t12, constitutional_secularism__principled_intervention_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(cons_su_t24, constitutional_secularism__principled_intervention_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(cons_su_t36, constitutional_secularism__principled_intervention_reading, suppression_requirement, 36, 0.42).
narrative_ontology:measurement(cons_su_t48, constitutional_secularism__principled_intervention_reading, suppression_requirement, 48, 0.45).
narrative_ontology:measurement(cons_su_t60, constitutional_secularism__principled_intervention_reading, suppression_requirement, 60, 0.47).
narrative_ontology:measurement(cons_su_t70, constitutional_secularism__principled_intervention_reading, suppression_requirement, 70, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the constitutional_secularism kernel. strict_neutrality_reading authors near-zero state-interference extraction but a correspondingly weaker beneficiary structure for internally powerless coreligionists (the neutrality reading's cost falls on those who lack an internal remedy). reformist_reading authors higher extraction against religious autonomy claimants generally, since it treats religious-autonomy objections as categorically subordinate rather than testing them case-by-case. principled_intervention_reading (this story) sits between them: bounded, justificatory intervention with moderate extraction concentrated asymmetrically on politically weak minority communities. All three share the same underlying kernel (constitutional secularism) but instantiate structurally distinct constraints with distinct ε values, beneficiary/victim sets, and majoritarian-capture risk profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
