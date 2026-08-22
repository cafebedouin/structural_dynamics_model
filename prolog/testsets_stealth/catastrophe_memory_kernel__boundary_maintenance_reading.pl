% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Communal Mourning-Cycle as Membership Boundary
 *   domain: religious/collective-memory/social
 *
 * SUMMARY:
 *   A post-catastrophe diaspora community maintains an annual cycle of
 *   communal mourning rites — fixed fast days, memorial liturgies, home
 *   observances, and public ceremonies — administered by communal leadership
 *   and treated as obligatory for full membership. This story instantiates
 *   the boundary_maintenance_reading of the catastrophe_memory_kernel: the
 *   rite's operative function is drawing and policing the community's edge.
 *   Participation certifies membership; absence is sanctioned; households
 *   straddling the boundary are marked as partial; the surrounding population
 *   is defined as the outside of the circle of grief. Genuine coordination
 *   runs through the same structure — synchronized grief, predictable mutual
 *   aid, a reproduced historical account — so the arrangement coordinates and
 *   extracts at once. Within the kernel family, this reading's epsilon (0.58)
 *   reflects moderate extraction: boundary-maintenance costs (conformity
 *   pressure, exclusion, strained out-group relations) borne for real
 *   solidarity delivered. The sibling stories author different epsilons over
 *   their own framings — symbol_continuity_reading sees preservation with
 *   little coercion, survival_competence_reading reframes the costs as
 *   adaptive investment, and trauma_encoding_reading locates the harm in
 *   encoded grievance rather than autonomy — the readings disagree about who
 *   pays, not merely about whether the rite is good. Epsilon here is indexed
 *   to the standing arrangement under contest: the enforced communal mourning
 *   regime as this reading sees it.
 *
 * KEY AGENTS:
 *   - communal_leadership: agenda-setter (institutional/identity_locked) — administers calendar, liturgy, and sanctions; collects deference and memorial-season resources
 *   - observant_ingroup_members: primary beneficiary (organized/constrained) — receive belonging, mutual-aid activation, and recognized standing
 *   - communal_charitable_institutions: secondary beneficiary (organized/constrained) — fundraising and enrollment ride the solidarity the rite mobilizes
 *   - mourning_form_dissenters: payer (moderate/constrained) — accept the community, dispute the obligatory form, attend under protest
 *   - intermarried_descendants: payer (moderate/constrained) — households straddling the boundary, marked as partial members
 *   - assimilating_youth: payer (moderate/mobile) — younger members for whom annual proof-of-belonging competes with lives lived outside
 *   - out_group_neighbor_communities: excluded (powerful/arbitrage) — defined as the outside of the grief-circle, not consulted
 *   - comparative_religion_scholars: analytical observer — documents the boundary-function without bearing or collecting from it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Communal Mourning-Cycle as Membership Boundary").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious/collective-memory/social").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, 'e14bdbd9-2af1-4744-a46e-d74f6c86418c').
narrative_ontology:cs_kernel_codification('e14bdbd9-2af1-4744-a46e-d74f6c86418c', formalized).
narrative_ontology:cs_authority_grounding('e14bdbd9-2af1-4744-a46e-d74f6c86418c', lineage).
narrative_ontology:cs_interpretation_layer_present('e14bdbd9-2af1-4744-a46e-d74f6c86418c').
narrative_ontology:cs_reading_relation('e14bdbd9-2af1-4744-a46e-d74f6c86418c', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e14bdbd9-2af1-4744-a46e-d74f6c86418c', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e14bdbd9-2af1-4744-a46e-d74f6c86418c', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('e14bdbd9-2af1-4744-a46e-d74f6c86418c', foundational, membership_constituted_by_mourning_participation).
narrative_ontology:cs_axiom_status(membership_constituted_by_mourning_participation, holdable).
narrative_ontology:cs_axiom_grounding('e14bdbd9-2af1-4744-a46e-d74f6c86418c', membership_constituted_by_mourning_participation, conventional).
narrative_ontology:cs_axiom('e14bdbd9-2af1-4744-a46e-d74f6c86418c', foundational, boundary_clarity_outweighs_grief_autonomy).
narrative_ontology:cs_axiom_status(boundary_clarity_outweighs_grief_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('e14bdbd9-2af1-4744-a46e-d74f6c86418c', boundary_clarity_outweighs_grief_autonomy, instrumental).
narrative_ontology:cs_reference_frame('e14bdbd9-2af1-4744-a46e-d74f6c86418c', survivor_generation_binding_rite).
narrative_ontology:cs_drift_state('e14bdbd9-2af1-4744-a46e-d74f6c86418c', third_generation_present, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e14bdbd9-2af1-4744-a46e-d74f6c86418c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, observant_ingroup_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, communal_charitable_institutions).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, mourning_form_dissenters).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, intermarried_descendants).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, assimilating_youth).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_neighbor_communities).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_solidarity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the memorial calendar, fixes the liturgy, decides who may publicly mourn and who leads each observance, and administers the consequences of non-participation — reduced honors, exclusion from lay roles, public censure. Their standing, livelihood, and self-understanding are constituted by administering the rite; stepping back from enforcement would dissolve the authority the rite sustains. Collects deference, control of communal resources, and first claim on memorial-season donations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).

% Participate fully in the annual mourning cycle: fast days, memorial services, home observances. Receive belonging, a settled account of their history, predictable activation of mutual-aid networks at times of personal loss, and recognized standing as full members. Leaving the practice would mean surrendering the community's entire recognition structure; most cannot picture themselves outside it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, observant_ingroup_members, beneficiary,
    organized, biographical, constrained, national).

% Schools, burial societies, and welfare funds whose fundraising peaks track the memorial calendar and whose enrollment depends on families remaining inside the boundary the rite marks. They do not run the observance but depend on the solidarity it mobilizes each season.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, communal_charitable_institutions, beneficiary,
    organized, generational, constrained, national).

% Members who accept the community but dispute the obligatory form — who want private grief, object to the political framing layered onto the mourning, or find the annual public performance hollow. They attend under protest or accept reduced standing; voicing the objection risks being read as disloyalty to the dead.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, mourning_form_dissenters, payer,
    moderate, biographical, constrained, national).

% Descendants partnered with people outside the community. The rite marks them as partial: welcomed to attend, discouraged or barred from leading, discussed as a leakage concern. Their households straddle the line the rite draws, and the closer their ties outside, the more each year's demands cost them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, intermarried_descendants, payer,
    moderate, biographical, constrained, national).

% Younger members pulled by mainstream schooling, careers, and marriage markets. The rite demands visible annual participation as proof of continued belonging, and each year that demand competes more directly with lives increasingly lived outside. They can leave — the surrounding society imposes no barrier — but leaving forfeits family standing and inherited meaning.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, assimilating_youth, payer,
    moderate, biographical, mobile, national).

% The surrounding populations whose own dead are not mourned in this calendar and whose main appearance in the rite is as the contrast class against which the community's distinctiveness is defined. They are not consulted on the observance and would contest being cast as the outside of a grief-circle; they bear no material levy but live adjacent to a boundary drawn at their relational expense.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_neighbor_communities, excluded,
    powerful, generational, arbitrage, national).

% Study the rite comparatively, documenting how mourning practices mark membership across traditions and eras. They take no part in the observance and can describe the boundary-function without bearing or collecting anything from it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes grief into a fixed communal calendar and liturgy so that loss is processed together, mutual aid activates predictably at bereavement, and a shared account of the catastrophe is reproduced across generations without depending on any individual's initiative.
% TRANSFER_FUNCTION: Moves attendance-time, expressive conformity, and memorial-season donations from individual members to the communal center; moves recognized full-membership standing to compliant participants and withholds it from the non-compliant and the partially affiliated; defines the surrounding population as outside the circle of obligation.
% ABSENT_VOICES: Out-group neighbor communities are not consulted and would contest being cast as the boundary's outside. Former members who left, and intermarried households that stopped attending, would testify to the costs of the marking but are no longer in the room where the rite is planned.
% DISAPPEARANCE_RATIONALE: Without the enforced common rite, grief would privatize within a generation, mutual-aid activation would lose its predictable trigger, the memorial calendar would fragment into household customs, and the community's distinctiveness marker would blur — intermarriage patterns and institutional affiliation would shift measurably within decades.
% FOUNDING_PROBLEM: After the catastrophe, a decimated community had to reconstitute itself: process mass death without dissolving into the surrounding society that had failed it, bind scattered survivors into one obligated body, and keep the dead from sliding into a generic past.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the catastrophe corroborate the founding rupture, and demographers outside the community corroborate that distinctiveness-decay through assimilation is a real ongoing pressure. No external party attests that today's enforcement intensity matches present danger: sociologists studying the community characterize the boundary-work as exceeding documented threat levels. The corroboration covers the problem's past reality, not the current dosage.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 — moderate: the rite transfers real goods (attendance, expressive conformity, donations, standing) asymmetrically, but delivers real solidarity in return, so net extraction sits well below a predatory arrangement. Suppression 0.62 reflects enforcement that is overwhelmingly social rather than legal: honor-lists, exclusion from lay roles, censure of the intermarried, and the framing of non-mourning as betrayal of the dead. Theater_ratio 0.26: the grief is predominantly sincere, but the public-ceremony layer grows more performative as the founding generation passes and attendance becomes a signal rather than an overflow. Accessibility_collapse 0.45: alternatives genuinely persist — private mourning, secular commemoration, quiet exit — but the communal alternative collapses once one understands the rite governs membership standing, not merely grief expression. Resistance 0.40: quiet non-attendance, reform liturgies, and scholarly critique, rarely open confrontation. The temporal series shares one six-point grid; all three tracked metrics rise gently as the founding generation's spontaneous grief gives way to administered observance and enforcement formalizes. Claimed type tangled_rope is stated from the structure — real coordination function, asymmetric costs, active enforcement — and the metrics were authored independently of that claim.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat the rite is the community's spine — the thing that makes the collectible 'we' exist — and enforcement reads as stewardship. From the dissenter and youth seats the same structure operates as a standing test they did not consent to, renewed annually. From the out-group seat it is a wall described as a circle. The engine computes these per-seat classifications from power, exit, and directionality; the divergence between the leadership's computed experience and the payers' is the point of the surface, not something the authored claim resolves.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: communal_leadership and observant_ingroup_members are declared beneficiaries, pulling their d toward the subsidized end — leadership most strongly, since it collects the positional goods directly. Charitable institutions benefit incidentally through mobilized solidarity. Dissenters, intermarried descendants, and assimilating youth are declared victims with differing exits: constrained exit keeps dissenters and intermarried households near the full-target end, while the youth's realistic mobility damps their d somewhat. Out-group neighbor communities are declared victims for the asymmetry gate, but their burden is definitional adjacency rather than transferred cost, and their arbitrage-grade non-membership would push the derived d toward the beneficiary end — wrong in the other direction — so an explicit override pins the powerful atom at 0.55: mildly burdened, neither subsidized nor harvested.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconstituting a decimated community after rupture — was real and is externally corroborated; three generations later its acute phase has passed, while the arrangement persists with its center of gravity shifted from grief-processing to membership-testing. Classifying this as tangled_rope rather than snare prevents the misread that the solidarity is cover: mutual aid and shared mourning are delivered, and many members are net beneficiaries. Classifying it as rope would erase the asymmetric ledger — dissenters, intermarried households, and the defined-outside pay for a cohesion others consume. The R5 fields record the contested status: the founding problem is not dead (assimilation pressure is real and externally corroborated), but the current enforcement dosage exceeds documented threat — precisely the mismatch pattern the genealogy interview exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the persistence of the mourning-practice better explained by boundary-enforcement (this reading) than by symbolic continuity, survival-competence transmission, or trauma encoding (the sibling readings of catastrophe_memory_kernel)?',
    'Compare communities where boundary payoff varies (open versus closed membership regimes) while the rite persists: if observance holds steady where the boundary yields nothing, boundary-enforcement is not the load-bearing function.',
    'If a sibling reading is load-bearing, the victim set shifts — under trauma_encoding the harmed are those whose grief stays encoded rather than members whose autonomy is constrained — and this story''s extraction estimate redistributes accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which function of the shared kernel explains the rite''s persistence.').

omega_variable(
    suppression_internalization_split,
    'Is the measured conformity pressure structural (honor-lists, exclusion from lay roles, censure) or internalized (members experience non-mourning as betrayal of the dead)?',
    'Post-exit trajectory of leavers: if former members resume autonomous mourning practice freely once outside, the prohibition was structural; if they carry guilt and compulsive observance after exit, a large share was internalized.',
    'An internalized share raises effective suppression above the structural measure and survives formal liberalization — sanctions could be abolished while compliance persists unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized components of the conformity pressure.').

omega_variable(
    cohesion_delivery_reality,
    'Does the rite actually deliver measurable solidarity — mutual-aid activation, retention, crisis mobilization — or is cohesion asserted by the leadership seat while the observable outputs shrink?',
    'Longitudinal comparison of mutual-aid response times, member retention, and donation flows across communities with varying rite-intensity, controlling for size and wealth.',
    'If cohesion delivery is weak, the coordination side of the ledger thins and the arrangement drifts toward extraction operating behind a ceremonial front.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohesion_delivery_reality, empirical, 'Whether the solidarity benefit is real or asserted.').

omega_variable(
    outgroup_harm_weighting,
    'Does being defined as the outside of the grief-circle impose a genuine cost on out-group neighbor communities, or is the harm negligible definitional residue?',
    'Survey and relational data from adjacent communities: measured social distance, reciprocity breakdowns, and incidents traceable to the rite''s contrast-class framing.',
    'If the harm is substantial, the victim set carries more weight and effective extraction rises; if negligible, the out-group declaration functions mainly as evidence that the boundary exists rather than as a cost ledger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(outgroup_harm_weighting, conceptual, 'Magnitude of harm borne by the defined-outside populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmk_bnd_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cmk_bnd_tr_t0, observed).
narrative_ontology:measurement(cmk_bnd_tr_t16, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement_basis(cmk_bnd_tr_t16, observed).
narrative_ontology:measurement(cmk_bnd_tr_t32, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement_basis(cmk_bnd_tr_t32, observed).
narrative_ontology:measurement(cmk_bnd_tr_t48, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 48, 0.22).
narrative_ontology:measurement_basis(cmk_bnd_tr_t48, observed).
narrative_ontology:measurement(cmk_bnd_tr_t64, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 64, 0.24).
narrative_ontology:measurement_basis(cmk_bnd_tr_t64, observed).
narrative_ontology:measurement(cmk_bnd_tr_t80, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement_basis(cmk_bnd_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(cmk_bnd_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cmk_bnd_be_t0, observed).
narrative_ontology:measurement(cmk_bnd_be_t16, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement_basis(cmk_bnd_be_t16, observed).
narrative_ontology:measurement(cmk_bnd_be_t32, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement_basis(cmk_bnd_be_t32, observed).
narrative_ontology:measurement(cmk_bnd_be_t48, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 48, 0.54).
narrative_ontology:measurement_basis(cmk_bnd_be_t48, observed).
narrative_ontology:measurement(cmk_bnd_be_t64, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 64, 0.56).
narrative_ontology:measurement_basis(cmk_bnd_be_t64, observed).
narrative_ontology:measurement(cmk_bnd_be_t80, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement_basis(cmk_bnd_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(cmk_bnd_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(cmk_bnd_su_t0, observed).
narrative_ontology:measurement(cmk_bnd_su_t16, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement_basis(cmk_bnd_su_t16, observed).
narrative_ontology:measurement(cmk_bnd_su_t32, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement_basis(cmk_bnd_su_t32, observed).
narrative_ontology:measurement(cmk_bnd_su_t48, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement_basis(cmk_bnd_su_t48, observed).
narrative_ontology:measurement(cmk_bnd_su_t64, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 64, 0.6).
narrative_ontology:measurement_basis(cmk_bnd_su_t64, observed).
narrative_ontology:measurement(cmk_bnd_su_t80, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement_basis(cmk_bnd_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the community's mourning-practice' decomposes into four structurally distinct constraints instantiating the catastrophe_memory_kernel: this boundary_maintenance_reading (rite as membership-edge policing, moderate epsilon, victims include autonomy-bearers and the defined-outside), symbol_continuity_reading (rite as symbolic preservation, low epsilon), survival_competence_reading (rite as transmitted persecution-survival capacity, costs reframed as adaptive investment), and trauma_encoding_reading (rite as intergenerational warning system, harm located in encoded grievance). Each is a separate story with its own epsilon, beneficiary/victim structure, and classification; they are linked here because the upstream readings are cited as justification for the enforcement this reading describes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__boundary_maintenance_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
