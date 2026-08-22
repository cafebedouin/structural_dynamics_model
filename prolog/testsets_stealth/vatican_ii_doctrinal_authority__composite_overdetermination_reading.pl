% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Unified Reform Package — Composite Overdetermination Reading
 *   domain: religious/institutional/hermeneutic
 *
 * SUMMARY:
 *   This story instantiates the composite-overdetermination reading of the
 *   Vatican II doctrinal-authority kernel. The constraint modeled is not any
 *   single doctrinal content but the standing post-conciliar arrangement
 *   whereby the Council's heterogeneous outputs — liturgical reform, the
 *   ecumenical reorientation, the ecclesiological shift toward collegiality,
 *   and the political-theological accommodation on religious liberty — are
 *   administered as one indivisible package of authority. Under this reading
 *   the packaging is itself the operative structure: it converts what could
 *   be component-wise negotiation into all-or-nothing loyalty positioning,
 *   concentrates implementation discretion in the Roman center, and preserves
 *   the conciliar texts' reconcilable-opposites ambiguities because the
 *   ambiguities are what allow every faction to affirm the whole while the
 *   center decides what the whole means in practice. The epsilon referent is
 *   that standing packaging arrangement, assessed by this reading's own
 *   lights; the doctrinal components themselves are separate stories in the
 *   constraint family this reading implies (see
 *   network.dual_formulation_note). KEY AGENTS (by structural relationship):
 *   papal_magisterium — agenda-setter and principal collector
 *   (institutional/identity_locked); curial_dicastery_officials — secondary
 *   beneficiary (institutional/constrained); traditionalist_communities —
 *   primary target (organized/trapped); component_specific_theologians —
 *   target (moderate/constrained); local_episcopal_conferences — ambivalent
 *   target-beneficiary (institutional/constrained);
 *   lay_liturgical_participants — diffuse target-beneficiary
 *   (powerless/constrained); ecumenical_partners — excluded party
 *   (organized/mobile); conciliar_historians — analytical observer
 *   (analytical/analytical).
 *
 * KEY AGENTS:
 *   - papal_magisterium: agenda-setter and principal collector — defines what the Council obligates and decides what the package means in practice
 *   - curial_dicastery_officials: secondary beneficiary — staff the interpretation and enforcement machinery whose remit depends on central adjudication
 *   - traditionalist_communities: primary target — seek component-wise relief and receive or lose it by unilateral central act
 *   - component_specific_theologians: target — scholarship judged against package-fidelity rather than component-level merit
 *   - local_episcopal_conferences: ambivalent target-beneficiary — gained collegiality, lost local adaptation room
 *   - lay_liturgical_participants: diffuse target-beneficiary — receive the reformed rites, bear the churn of reversal
 *   - ecumenical_partners: excluded party — affected by the package's instability with no seat in its administration
 *   - conciliar_historians: analytical observer — document the assembly of the package from the drafting record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.7).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.74).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Unified Reform Package — Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "religious/institutional/hermeneutic").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '545084e9-c6fb-41e1-a7b2-a66631ccb3b9').
narrative_ontology:cs_kernel_codification('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', fixed_text).
narrative_ontology:cs_authority_grounding('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', extraction).
narrative_ontology:cs_interpretation_layer_present('545084e9-c6fb-41e1-a7b2-a66631ccb3b9').
narrative_ontology:cs_reading_relation('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_axiom('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', foundational, component_wise_assessment_required).
narrative_ontology:cs_axiom_status(component_wise_assessment_required, holdable).
narrative_ontology:cs_axiom_grounding('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', component_wise_assessment_required, empirically_contingent).
narrative_ontology:cs_axiom('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', foundational, ambiguity_structurally_functional).
narrative_ontology:cs_axiom_status(ambiguity_structurally_functional, holdable).
narrative_ontology:cs_axiom_grounding('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', ambiguity_structurally_functional, empirically_contingent).
narrative_ontology:cs_reference_frame('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', indivisible_conciliar_package_authority).
narrative_ontology:cs_drift_state('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', contemporary_synodal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('545084e9-c6fb-41e1-a7b2-a66631ccb3b9', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, papal_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, curial_dicastery_officials).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, component_specific_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, local_episcopal_conferences).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, lay_liturgical_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, local_episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, lay_liturgical_participants).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_indivisibility_presumption).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, hermeneutic_of_reform_in_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines what 'the Council' obligates and decides what the package means in practice: issues binding interpretations through the dicasteries, selects which conciliar impulses to implement and which to restrain, and receives every internal dispute as a question of package-fidelity. Because the office's authority is fused with the conciliar settlement, abandoning the indivisible-package framing would mean repudiating its own claim to adjudicate — the office cannot exit the structure it administers.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, papal_magisterium, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, papal_magisterium, beneficiary).

% Staff the congregations and councils that police implementation: issue notifications, assess theologians, regulate liturgical translations and permissions. Careers, jurisdiction, and publishing prerogatives depend on the package remaining centrally adjudicated; component-wise devolution to local churches or scholarly bodies would shrink their remit directly.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, curial_dicastery_officials, beneficiary,
    institutional, biographical, constrained, global).

% Networks of fraternities, institutes, and parishes attached to pre-conciliar liturgical and doctrinal forms. They live under the post-conciliar code while rejecting specific components, and their recurring request is component-wise relief — access to earlier forms without repudiation of the rest. Relief arrives or is withdrawn by unilateral central act (broadly granted in 2007, restricted in 2021). Leaving the communion would dissolve the identity they exist to preserve, so they persist inside a package they cannot sign whole.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_communities, payer,
    organized, biographical, trapped, global).

% Research and teach on single questions — the development of religious-liberty doctrine, liturgical history, episcopal collegiality — under a regime where conclusions are judged against package-fidelity rather than component-level scholarship. Teaching requires an ecclesiastical mandate; careers depend on staying inside lines drawn by an authority they may assess piece by piece but must not publicly split.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, component_specific_theologians, payer,
    moderate, biographical, constrained, continental).

% Gained durable doctrinal standing from one component of the package (the collegiality shift) and bear costs from others: mandated liturgical uniformity overriding local rite adaptations, doctrinal assessment of conference documents, and implementation directives that arrive without consultation. They cannot unbundle locally because the package is administered from the center, and their collegiate gains are exercised only at the center's pleasure.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, local_episcopal_conferences, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, local_episcopal_conferences, beneficiary).

% Worship in the reformed rites — a genuine received good — while bearing the churn of discretionary reversal: parishes gaining and losing access to earlier liturgical forms within a single decade. They hold no seat in how the package is interpreted; their lived experience is cited by every faction but owned by none, and their realistic options are attendance, complaint, or departure.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, lay_liturgical_participants, payer,
    powerless, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, lay_liturgical_participants, beneficiary).

% Non-Catholic churches and communions engaged by the conciliar decree on ecumenism. Their bilateral dialogues presuppose a Catholic counterpart speaking for a settled position, yet the package's internal contests repeatedly reopen what that position is. They have no seat in the Catholic administration of the package; their recourse is to suspend, slow, or continue dialogue.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners, excluded,
    organized, generational, mobile, global).

% Reconstruct the drafting process: the successive schemas, the redactional widening of contested passages into reconcilable opposites, the printed reservations of the conciliar minority. They document that the package was assembled under acceptance-maximizing constraints and that its components carry different degrees of change. They neither collect from the arrangement nor pay into it.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, papal_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A global communion spanning thousands of cultures needed, after a century of centrifugal pressure, a common liturgy, a common posture toward other Christians, and a coherent account of its own authority — one council produced these once and centrally instead of per-national-church improvisation. The packaging added a second-order coordination service: rival factions would accept the outcomes only if each could read its priorities into deliberately broad formulas, so the bundle held the communion together while it changed.
% TRANSFER_FUNCTION: Moves interpretive authority and loyalty-obligation upward: assent owed to the whole package regardless of component-specific judgment flows from local churches, theologians, and liturgical communities to the Roman center, along with discretion over what implementation means — a discretion the center exercises selectively, granting broad access to earlier liturgical forms in 2007 and withdrawing it in 2021.
% ABSENT_VOICES: The conciliar minority's published reservations were absorbed into the package rather than answered item by item; ecumenical observers spoke at the council but have no seat in its subsequent administration; the lay faithful had no vote in 1965 and have none now; clergy attached to earlier forms after 1988 address the center only through irregular channels.
% DISAPPEARANCE_RATIONALE: If the packaging vanished overnight — if 'the Council' stopped functioning as a single authority and each component had to be defended, amended, or repealed on its own record — the church's internal politics would reorganize around component coalitions within a few years; liturgical pluralism would likely widen permanently; the center's interpretive rent would evaporate with the all-or-nothing assent it taxes; and ecumenical counterparts would negotiate with positions that could stop moving beneath them.
% FOUNDING_PROBLEM: Hold a worldwide communion together through a period of necessary but bitterly contested modernization: a narrowly drafted set of decrees would have been rejected by one faction or another and split the church, so the drafts were widened into reconcilable opposites and presented as one package every faction could sign.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the historiography of the drafting process (archival studies of the redactional widening of the schemas and the published relatio explanations), by the printed reservations of the conciliar minority itself, and by ecumenical partners' contemporaneous assessments that breadth was purchased for acceptance. No beneficiary attestation is relied upon.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.70 at interval end) reflects the packaging's core transfer: component-wise judgment is taxed into all-or-nothing assent, and implementation discretion accrues to the center. Suppression (0.74) is higher still because the package's persistence depends on actively closing the component-wise exit — the 2021 restriction of earlier liturgical forms removed precisely the partial exit that had briefly existed — and on disciplinary machinery aimed at theologians whose component-level conclusions outrun package-fidelity. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream. Theater (0.44) is moderate: the underlying functions are real (a shared liturgy exists, ecumenical dialogue occurs), but a growing share of activity is performative maintenance — anniversary hermeneutics, invocations of 'the Council' as a unitary speaker, and fidelity rhetoric substituting for component-level engagement. Accessibility collapse (0.55): within the framework, component-wise adherence is structurally foreclosed — one cannot officially be Catholic-and-only-part-II — but exits persist at the margins (irregular communities, Eastern Catholic liturgical exception, individual departure), so alternatives are narrowed rather than eliminated. Resistance (0.60) is substantial and bidirectional: traditionalist networks contest from one side, progressive theological and synodal movements from the other. The temporal series run on one shared grid; the 2007 dip and 2021 spike track the grant and withdrawal of the partial liturgical exit. The claimed type (tangled_rope) is stated independently of these metrics: the arrangement solves a real coordination problem for a global communion while extracting through the same structure that solves it.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the package is the condition of its own authority: unbundling would dissolve the office's role as sole adjudicator of what the Council means, so the seat experiences the structure as self-maintenance, not extraction. From the trapped payer seat (traditionalist communities) the same structure operates as loyalty taxation with discretionary relief. From the constrained professional seats (theologians, curia apart) it operates as career-conditioned speech regulation. From the excluded seat (ecumenical partners) it operates as counterparty instability. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The papal magisterium sits nearest the beneficiary pole: it collects the assent-tax and owns the discretion, and its identity-lock amplifies rather than dampens its position. Curial officials collect second-order rents (jurisdiction, careers) with constrained exit. Traditionalist communities sit nearest the target pole: trapped by identity, organized enough to resist, taxed whenever they seek component relief. Theologians are targets with somewhat better exit (academic migration, secular publication). Episcopal conferences are genuinely mixed — the derivation reads their victim declaration toward the target pole, and their secondary beneficiary role (collegiality gains) tempers it; the residual ambivalence is structural, not noise. Lay participants are diffuse targets with a real received benefit (secondary beneficiary role), which places them short of full-target. Ecumenical partners are excluded rather than coordinated: their exclusion is a precondition of the packaging, since a seat for them would force component-wise specification of the Catholic position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving unity through contested modernization — is not dead: every pontificate still faces fragmentation pressure from both flanks, which is why founding_problem_status is contested rather than dead. The packaging therefore cannot be dismissed as a piton (its administrator actively maintains it and profits from it, which is snare-side evidence) nor celebrated as a pure rope (the coordination it performs is inseparable from the loyalty tax it levies). The mandatrophy question this story keeps open: does the unity-preservation mandate still describe the packaging's function, or has the packaging begun manufacturing the disputes it manages — converting component-level disagreements into package-level loyalty crises that only the center can resolve? The mismatch consumer should watch founding_problem_status (contested) against disappearance_verdict (world_rearranges): the arrangement is load-bearing, but whether it bears the load it was built for is precisely what the factions dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the vatican_ii_doctrinal_authority kernel — the composite_overdetermination_reading. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Not resolvable by data alone: the disagreement is located in whether ''the Council'' names one authority-object or several separable ones. Resolution would come from the corpus adopting or rejecting component-wise assessment as the frame for measuring the conciliar settlement.',
    'Under continuity_reading the packaging-extraction claim dissolves (organic development carries no bundling rent and epsilon collapses toward coordination cost); under rupture_traditionalist_reading extraction relocates into specific doctrinal contents rather than the packaging; under rupture_progressive_reading the packaging becomes scaffolding for further reform rather than a rent-preserving structure. This story''s epsilon (0.70) is valid only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega recording the kernel, this reading, the sibling structural deltas, and the location of the disagreement.').

omega_variable(
    component_separability,
    'Are the liturgical, ecumenical, ecclesiological, and religious-liberty components actually governable separately, or are they causally entangled such that unbundling is impossible?',
    'Natural experiments in partial separation: Summorum Pontificum (2007-2021) as a partial liturgical unbundling and its observed effects on cohesion; the permanent liturgical exception of the Eastern Catholic churches; diocesan-level variation where toleration persisted.',
    'If separable, the packaging layer is pure rent and this story''s extractiveness requires no downward correction; if entangled, part of the measured extraction is irreducible coordination cost and epsilon should be read toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_separability, empirical, 'Whether the bundle is a contingent administrative artifact or a causal necessity.').

omega_variable(
    ambiguity_intentionality,
    'Were the conciliar texts'' reconcilable-opposites formulations deliberate instruments for maximizing acceptance (this reading''s claim) or honest expressions of questions the council fathers had not resolved?',
    'Drafting-process archives: successive schemas, coordinating-commission correspondence, and relatio explanations showing whether breadth was engineered or emergent.',
    'Deliberate ambiguity supports extraction-by-design at the packaging layer; sincere unresolvedness recasts the package as transitional support awaiting clarification, lowering persistence-weighted extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentionality, empirical, 'Intentionality of the conciliar ambiguities.').

omega_variable(
    enforcement_symmetry,
    'Does the enforcement machinery police component-deviation symmetrically across factions, or does it fall disproportionately on traditionalist deviation?',
    'Compare the 2021 restriction of earlier liturgical forms with the handling of progressive component-deviation over the same period (doctrinal assessment of the German synodal pathway, treatment of dissenting moral theology), controlling for canonical severity of the underlying acts.',
    'Symmetric enforcement supports the tangled-rope reading; documented asymmetry would indicate the packaging operates as selective enforcement, drifting the computed classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_symmetry, empirical, 'Symmetry of enforcement across factions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2_composite_reading_tr_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.14).
narrative_ontology:measurement_basis(v2_composite_reading_tr_t1962, observed).
narrative_ontology:measurement(v2_composite_reading_tr_t1970, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1970, 0.26).
narrative_ontology:measurement_basis(v2_composite_reading_tr_t1970, observed).
narrative_ontology:measurement(v2_composite_reading_tr_t1980, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1980, 0.33).
narrative_ontology:measurement_basis(v2_composite_reading_tr_t1980, observed).
narrative_ontology:measurement(v2_composite_reading_tr_t1990, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1990, 0.36).
narrative_ontology:measurement_basis(v2_composite_reading_tr_t1990, observed).
narrative_ontology:measurement(v2_composite_reading_tr_t2000, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(v2_composite_reading_tr_t2000, observed).
narrative_ontology:measurement(v2_composite_reading_tr_t2007, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2007, 0.32).
narrative_ontology:measurement_basis(v2_composite_reading_tr_t2007, observed).
narrative_ontology:measurement(v2_composite_reading_tr_t2021, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2021, 0.43).
narrative_ontology:measurement_basis(v2_composite_reading_tr_t2021, observed).
narrative_ontology:measurement(v2_composite_reading_tr_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.44).
narrative_ontology:measurement_basis(v2_composite_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(v2_composite_reading_be_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.22).
narrative_ontology:measurement_basis(v2_composite_reading_be_t1962, observed).
narrative_ontology:measurement(v2_composite_reading_be_t1970, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement_basis(v2_composite_reading_be_t1970, observed).
narrative_ontology:measurement(v2_composite_reading_be_t1980, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1980, 0.56).
narrative_ontology:measurement_basis(v2_composite_reading_be_t1980, observed).
narrative_ontology:measurement(v2_composite_reading_be_t1990, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1990, 0.59).
narrative_ontology:measurement_basis(v2_composite_reading_be_t1990, observed).
narrative_ontology:measurement(v2_composite_reading_be_t2000, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement_basis(v2_composite_reading_be_t2000, observed).
narrative_ontology:measurement(v2_composite_reading_be_t2007, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2007, 0.53).
narrative_ontology:measurement_basis(v2_composite_reading_be_t2007, observed).
narrative_ontology:measurement(v2_composite_reading_be_t2021, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement_basis(v2_composite_reading_be_t2021, observed).
narrative_ontology:measurement(v2_composite_reading_be_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.7).
narrative_ontology:measurement_basis(v2_composite_reading_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(v2_composite_reading_su_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.28).
narrative_ontology:measurement_basis(v2_composite_reading_su_t1962, observed).
narrative_ontology:measurement(v2_composite_reading_su_t1970, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement_basis(v2_composite_reading_su_t1970, observed).
narrative_ontology:measurement(v2_composite_reading_su_t1980, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement_basis(v2_composite_reading_su_t1980, observed).
narrative_ontology:measurement(v2_composite_reading_su_t1990, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1990, 0.64).
narrative_ontology:measurement_basis(v2_composite_reading_su_t1990, observed).
narrative_ontology:measurement(v2_composite_reading_su_t2000, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(v2_composite_reading_su_t2000, observed).
narrative_ontology:measurement(v2_composite_reading_su_t2007, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2007, 0.48).
narrative_ontology:measurement_basis(v2_composite_reading_su_t2007, observed).
narrative_ontology:measurement(v2_composite_reading_su_t2021, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2021, 0.72).
narrative_ontology:measurement_basis(v2_composite_reading_su_t2021, observed).
narrative_ontology:measurement(v2_composite_reading_su_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.74).
narrative_ontology:measurement_basis(v2_composite_reading_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_liturgical_reform_component).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_religious_liberty_component).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_ecumenical_component).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition note: the unified label 'Vatican II' covers at least three structurally distinct sub-constraints with independent epsilon values — the liturgical reform (vatican_ii_liturgical_reform_component), the religious-liberty accommodation (vatican_ii_religious_liberty_component), and the ecumenical reorientation (vatican_ii_ecumenical_component) — plus this packaging-layer story, the only member where the bundling itself is the constraint. The sibling kernel readings (continuity, rupture-progressive, rupture-traditionalist) each measure the whole corpus under a single-epsilon assumption; this story exists because that assumption fails under this reading. Upstream/downstream: the sibling readings are upstream in visibility (public debate runs through them) while the component stories are downstream decompositions this reading generates; edges above link both sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
