% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto as Revealed Temporal Suspension of Plural Marriage (Endogenous Reinterpretation Reading)
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   On October 6, 1890, Wilford Woodruff, President of the Church of Jesus
 *   Christ of Latter-day Saints, issued the Manifesto suspending the
 *   contraction of new plural marriages, and the church sustained it as
 *   revelation. This story authors the standing arrangement as the endogenous
 *   reading holds it: a genuine prophetic reinterpretation in which God
 *   revealed a temporal suspension of an eternal principle so that the
 *   church's salvific mission — its temples, its legal existence, its
 *   worldwide gathering — would survive. The arrangement coordinates the
 *   covenant community around the new directive: members abstain from new
 *   plural marriages, the institution disciplines those who continue the
 *   practice, the doctrinal principle remains in canon awaiting restoration,
 *   and the minority that keeps the original reading is ultimately separated
 *   from the fellowship. The interval runs from the Manifesto to the
 *   fundamentalist separation of the early 1930s, when enforcement of the
 *   suspension reached its mature form. This is one reading of a contested
 *   kernel; the sibling readings are separate constraint stories linked
 *   through network.affects_constraints (see commentary.kernel_context and
 *   the kernel_reading_contestation omega).
 *
 * KEY AGENTS:
 *   - church_institution (First Presidency and Quorum of the Twelve): agenda-setter and primary beneficiary — administers the suspension, enforces discipline, collects survival, temple continuity, and legal standing
 *   - rank_and_file_membership: beneficiary and cost-bearer — sustained the directive, received the preserved church and open temples, bore the closing of the covenant practice
 *   - fundamentalist_dissenters: primary target — bore excommunication and community severance for maintaining the original reading
 *   - manifesto_dissenting_apostles: internal cost-bearers — resigned rather than comply with post-1904 enforcement
 *   - us_federal_government: external compliance-recipient — received the legal conformity the directive delivered
 *   - religious_historians: analytical observer — adjudicate the causal record from outside the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.35).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.55).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto as Revealed Temporal Suspension of Plural Marriage (Endogenous Reinterpretation Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious institutional history / commitment systems / political theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e0c32439-bf4a-41ca-a6ce-5fbe9a34590b').
narrative_ontology:cs_kernel_codification('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', fixed_text).
narrative_ontology:cs_authority_grounding('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', lineage).
narrative_ontology:cs_interpretation_layer_present('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b').
narrative_ontology:cs_reading_relation('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', foundational, manifesto_originated_in_divine_revelation).
narrative_ontology:cs_axiom_status(manifesto_originated_in_divine_revelation, holdable).
narrative_ontology:cs_axiom_grounding('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', manifesto_originated_in_divine_revelation, theological).
narrative_ontology:cs_axiom('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', foundational, prophet_may_temporarily_suspend_eternal_principle).
narrative_ontology:cs_axiom_status(prophet_may_temporarily_suspend_eternal_principle, holdable).
narrative_ontology:cs_axiom_grounding('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', prophet_may_temporarily_suspend_eternal_principle, theological).
narrative_ontology:cs_axiom('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', secondary, salvific_mission_preservation_justifies_suspension).
narrative_ontology:cs_axiom_status(salvific_mission_preservation_justifies_suspension, holdable).
narrative_ontology:cs_axiom_grounding('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', salvific_mission_preservation_justifies_suspension, instrumental).
narrative_ontology:cs_reference_frame('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', continuing_revelation_prophetic_frame).
narrative_ontology:cs_drift_state('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', contemporary_post_fundamentalist_schism, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('e0c32439-bf4a-41ca-a6ce-5fbe9a34590b', '2026-08-10T12:00:00Z').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, us_federal_government).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, manifesto_dissenting_apostles).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, prophetic_authority_supremacy).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, temporal_suspension_of_eternal_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The First Presidency and Quorum of the Twelve receive, proclaim, and administer the 1890 directive: they announce the suspension, define its scope, discipline members who contract new plural marriages after 1904, and retain the doctrinal principle in canon. What flows to them: the institution's legal existence, the return of escheated property after Utah statehood, uninterrupted temple operation, and an expanding worldwide missionary church. What flows from them: the binding interpretation of what the revelation requires and when discipline applies. Their alternatives are the widest of any seat — they resisted the federal government for a decade before 1890 and could have continued resisting or reshaped the directive's administration — and they received and announced the suspension as the resolution.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, beneficiary).

% The general membership sustains the directive at conference and reorganizes family and covenant expectations around it. What flows to them: a continuing church, open temples, legal safety, Utah statehood, and the assurance that the principle remains eternal and its blessings accessible. What flows from them: the closing of plural marriage to new participants — a practice their instruction had framed as required for the highest exaltation — and, for families formed under the older reading, the reordering of what they had understood as eternal covenant obligations. Leaving would mean forfeiting the community, the temples, and the salvation the church administers; a few left, most stayed.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, rank_and_file_membership, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__endogenous_reinterpretation_reading, rank_and_file_membership, payer).

% Members who hold that the covenant commitments of the 1880s remain in force and that no authority could set them aside. They continue or advocate new plural marriages after the directive, organize councils and communities to preserve the practice (the Council of Friends from 1929, settlements along the Utah-Arizona border and in northern Mexico), and are excommunicated beginning in earnest in the 1930s. What flows from them: membership, temple access, and community standing inside the church. What remains to them: the communities they build outside it and the conviction that the original reading is the true one. Abandoning that reading would dissolve the identity their families and communities are built on; from where they stand the choice is no choice at all.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters, payer,
    moderate, generational, identity_locked, regional).

% Apostles inside the governing quorum who had performed or authorized plural marriages between 1890 and 1904 and could not bring that conduct under the Second Manifesto's demand for renunciation. John W. Taylor and Matthias F. Cowley resign from the Quorum in 1905-1906 rather than comply, and Cowley later loses priesthood office. What flows from them: their positions, their quorum standing, and in one case priesthood office. They do not leave the church; they lose their seats inside it for holding that the earlier authorizations were valid.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, manifesto_dissenting_apostles, payer,
    powerful, generational, identity_locked, continental).

% The courts, Congress, and federal prosecutors spent the 1880s dismantling the church's legal position: the Reynolds ruling, the Edmunds-Tucker Act, escheatment of church property, incarceration of leaders, disfranchisement of members. The suspension delivers the conformity they demanded — plural marriage ends as an open practice, Utah is admitted with the church's cooperation, property is restored. The government's interest is conformity with federal law, not the church's internal warrant for the change; it holds the coercive levers throughout and can escalate or relax enforcement at will.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, us_federal_government, beneficiary,
    institutional, generational, arbitrage, national).

% Scholars working from Woodruff's journals, the 1891 Supreme Court brief, the post-Manifesto marriage record, and the federal enforcement archive. They reconstruct what happened, when enforcement tightened, and which accounts of the directive's origin the surviving documents support; they collect no benefit from the arrangement and bear none of its discipline.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed covenant community around a single binding directive at the moment its previous central practice became legally untenable: one prophetic answer to the question of what God now requires replaces thousands of private decisions about covenant continuation, preserving unified worship, temple access, and the missionary gathering.
% TRANSFER_FUNCTION: Moves covenant-obedience costs from individual believers — the closing of plural marriage to new participants and, for dissenters, membership itself — to secure collective institutional goods held by the whole body: legal existence, temple continuity, statehood, and mission capacity. Moves assurance in the return direction: the institution guarantees that the principle remains eternal and that its blessings remain accessible to the faithful.
% ABSENT_VOICES: The generation that bore the practice's heaviest costs — imprisoned polygamists, families of the underground years, colonists who emigrated to Mexico and Canada on the practice's promise — had largely died or aged out by 1890 and consented to the reversal only through descendants; the dissenters who kept the original reading held a voice until excommunication removed it, after which their objection persisted entirely outside the conversation; and the arrangement offered no seat from which a member could contest the directive without contesting prophetic authority itself, since the directive arrived clothed in the authority that would adjudicate objections to it.
% DISAPPEARANCE_RATIONALE: The twentieth-century LDS institutional form — Utah integration, unthreatened temples, a worldwide missionary church — and the entire fundamentalist Mormon world, which exists as a response to the directive, are constituted by this arrangement. Remove it overnight and the church's covenant boundary loses its twentieth-century definition, the fundamentalist communities lose the organizing grievance and priesthood claims around which they are built, and the settlement of the American West's religious politics (statehood, property restoration, the church's legal reconstitution) loses its pivot.
% FOUNDING_PROBLEM: The federal destruction crisis of 1879-1890: the Reynolds ruling, the Edmunds-Tucker Act's disincorporation and escheatment of church property, the incarceration of leaders, disfranchisement of members, and the denial of statehood together threatened the church's legal existence and with it the temple-based salvific mission. The arrangement was built to end that destruction by suspending the practice that triggered it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the church's own 1891 Supreme Court brief (arguing the practice had been abandoned as the condition for restoring escheated property), the federal enforcement record (prosecutions ceased, property returned after statehood in 1896), and the subsequent judicial eclipse of the anti-polygamy statutes all attest that the founding crisis is resolved; academic religious historians reconstruct the same arc from the enforcement archive. The church institution attests the directive remains binding on covenantal grounds independent of the crisis — no source outside the benefiting parties attests that covenantal claim, which is precisely where the sibling readings locate their disagreement.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   From this reading's seat the arrangement is genuine coordination around a new prophetic directive. Base extractiveness is authored low-moderate (0.35 at interval end, rising from 0.20) because the costs borne — the closing of the covenant practice to new participants and, for the dissenting margin, loss of membership — are assessed as legitimate obedience and boundary maintenance, while the delivered goods (legal survival, restored property, open temples, statehood, mission capacity) accrue to the whole community. Suppression (0.55) is authored as the raw structural enforcement the arrangement actually required, unscaled by power or scope: the 1890 Manifesto initially traveled on acceptance alone, the 1904 Second Manifesto built real disciplinary machinery, and by 1935 enforcement against continuing practitioners was mature; this reading assesses that enforcement as legitimate without denying its magnitude. Theater stays low (0.12, peak 0.15 at the Second Manifesto) because the reading holds the Manifesto genuine and the doctrine's retention consistent rather than performative; the modest 1896-1904 bump tracks the public/private gap of the transition years, which the Second Manifesto closed. Accessibility collapse (0.55) and resistance (0.50) reflect a field in which alternatives — continuation underground, emigration to Mexico and Canada, eventual separation into fundamentalist communities — remained partially available and were exercised at real cost, and the directive met persistent resistance from the minority that kept the original reading. The identity-coordination function is genuine from this seat — the directive maintains a covenant boundary the community itself sustains — though the sibling readings would re-read the same boundary maintenance as extraction cover, which is exactly the gaming risk the identity-coordination floor flags. All three measurement series share one seven-point grid (1890-1935) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the agenda-setter seat (church_institution), the directive is the instrument by which the community was preserved — received as revelation and administered as covenant. From the payer seat (fundamentalist_dissenters), the same structure is the mechanism that severed them from the community for refusing to re-read what they hold to be an eternal covenant. From the rank-and-file seat both are true at once: the directive delivered the church they remain in at the price of a practice their instruction had framed as required for the highest exaltation. Across institutions, the federal seat experiences the arrangement as policy compliance delivered under its own enforcement, holding arbitrage throughout, while the church seat experienced the choice as a one-time restructuring it could not repeat. Within the same governing quorum, apostles who had performed post-1890 marriages and apostles who had not faced the identical directive from identical office; the cost fell differentially because of conduct between 1890 and 1904, so the same-level differentiation runs through biography, not position. The engine computes per-seat classifications from the structural data; this story's claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   church_institution is the structural beneficiary and agenda-setter (collects survival, property restoration, temple continuity; controls the directive's administration — d near the beneficiary end). rank_and_file_membership sits near symmetric: genuine coordination benefit (preserved church, temple access, statehood) against real covenant costs. fundamentalist_dissenters are the targets (bear excommunication and severance; d near the target end). us_federal_government receives compliance incidentally without being a party to the covenant structure. manifesto_dissenting_apostles are institutional insiders who paid with their positions — the derivation reads them as targets within the institution. The identity-locked exits (fundamentalist_dissenters, manifesto_dissenting_apostles) run on covenant identity fusion: self-concept constituted through the covenant's original form, so exit is experienced as self-annihilation rather than relocation; if that frame broke — if a dissenter concluded the post-1886 authority chain was void — the lock would release and exit would reclassify as mobile. Beneficiary/victim declarations map directly: beneficiaries are church_institution and rank_and_file_membership; victims are fundamentalist_dissenters.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the federal destruction crisis) is dead — corroborated by statehood, restored property, and the statutes' judicial eclipse — while the arrangement persists and the world remains arranged around it. That mismatch is the live question this story refuses to reconcile: if the directive's binding force is crisis-contingent, the constraint is drifting toward inertial maintenance, with the retained doctrine shading into performance; if it is covenantal — as this reading holds — the arrangement is a live coordination whose mandate is the covenant itself, not the crisis. The temporal_suspension_coherence omega carries that question. Authoring the rope claim and the dead founding problem as independent facts lets the engine measure exactly this divergence instead of pre-resolving it in the reading's favor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story is the endogenous_reinterpretation_reading of the plural_marriage_mandate kernel — how would instantiating the exogenous_override_reading or the institutional_pragmatism_reading instead restructure this constraint''s beneficiary and victim sets, epsilon, and type?',
    'The sibling stories themselves: each reading is authored as its own constraint story over the same epsilon referent (the post-1890 suspension arrangement) and linked via network.affects_constraints; cross-reading comparison reads the three files'' structural deltas side by side.',
    'The exogenous_override_reading would seat the federal government as effective agenda-setter, raise epsilon substantially (abandonment of a divine requirement under coercion), and shift the type toward tangled_rope or snare; the institutional_pragmatism_reading would treat the revelation narrative itself as the legitimation vehicle, demote the vindicated propositions to contested status, and raise theater_ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the plural_marriage_mandate kernel this constraint instantiates and what the sibling readings would change structurally.').

omega_variable(
    causal_locus_of_manifesto,
    'Where is the operative cause of the 1890 directive located — in revelation received by the prophet (this reading''s claim), in federal coercion alone, or in strategic capitulation presented as revelation?',
    'Archival adjudication: Woodruff''s journal accounts of the revelation are the primary endogenous evidence but were produced inside the authority structure they support; the church''s 1891 Supreme Court brief arguing the practice had been abandoned, the record of post-Manifesto marriages, and contemporary apostolic testimony form the exogenous-leaning record; no account of the revelation''s content exists from a seat outside the benefiting authority structure.',
    'If the causal locus is coercion or strategy, this reading''s epsilon is understated, the vindicated propositions lose their warrant, and the constraint reclassifies toward extraction; if the locus is revelation, the coordination reading stands and discipline of dissenters is covenant boundary maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_locus_of_manifesto, empirical, 'The location of the disagreement between readings: causal attribution of the directive''s origin.').

omega_variable(
    temporal_suspension_coherence,
    'Is ''an eternal principle temporally suspended by revelation, doctrine retained, practice closed'' a coherent covenantal structure, or a frame that immunizes the directive against any possible revision evidence?',
    'Conceptual analysis within the tradition''s own criteria: whether the tradition states what evidence would distinguish suspension from abrogation, and whether the structure generates determinate obligations (for example, whether members could ever be released from the suspension without new revelation).',
    'If coherent, the arrangement is a live coordination whose mandate is the covenant itself; if immunized, the doctrine''s retention is performative maintenance and the constraint drifts toward inertial persistence with theater_ratio understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_suspension_coherence, conceptual, 'Whether the reading''s core temporal-suspension structure is coherent or revision-proof by construction.').

omega_variable(
    exit_lock_mechanism,
    'Is the constrained exit of dissenters and members structural (community severance, economic and geographic entanglement in an integrated Mormon commonwealth) or internalized (covenant identity fusion that makes exit unthinkable before any barrier is reached)?',
    'Post-separation trajectory of those who left: if former members who rejected the directive report the lock persisting after institutional severance, the internalized component is substantial; if their exit costs tracked material community ties that dissolved with departure, the mechanism is structural.',
    'If internalized, the arrangement''s effective hold exceeds the structural measure — those who exit carry the lock with them, and the fundamentalist communities'' persistence partly reflects carried identity rather than free re-formation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_lock_mechanism, empirical, 'Structural versus internalized mechanism of the exit hold on dissenters and members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1890, 1935).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.08).
narrative_ontology:measurement_basis(plur_tr_t1890, observed).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1896, 0.12).
narrative_ontology:measurement_basis(plur_tr_t1896, observed).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.15).
narrative_ontology:measurement_basis(plur_tr_t1904, observed).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.12).
narrative_ontology:measurement_basis(plur_tr_t1910, observed).
narrative_ontology:measurement(plur_tr_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement_basis(plur_tr_t1920, observed).
narrative_ontology:measurement(plur_tr_t1929, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1929, 0.1).
narrative_ontology:measurement_basis(plur_tr_t1929, observed).
narrative_ontology:measurement(plur_tr_t1935, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1935, 0.12).
narrative_ontology:measurement_basis(plur_tr_t1935, observed).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.2).
narrative_ontology:measurement_basis(plur_be_t1890, observed).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1896, 0.22).
narrative_ontology:measurement_basis(plur_be_t1896, observed).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.27).
narrative_ontology:measurement_basis(plur_be_t1904, observed).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.3).
narrative_ontology:measurement_basis(plur_be_t1910, observed).
narrative_ontology:measurement(plur_be_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.31).
narrative_ontology:measurement_basis(plur_be_t1920, observed).
narrative_ontology:measurement(plur_be_t1929, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1929, 0.33).
narrative_ontology:measurement_basis(plur_be_t1929, observed).
narrative_ontology:measurement(plur_be_t1935, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1935, 0.35).
narrative_ontology:measurement_basis(plur_be_t1935, observed).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.12).
narrative_ontology:measurement_basis(plur_su_t1890, observed).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1896, 0.18).
narrative_ontology:measurement_basis(plur_su_t1896, observed).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.38).
narrative_ontology:measurement_basis(plur_su_t1904, observed).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.46).
narrative_ontology:measurement_basis(plur_su_t1910, observed).
narrative_ontology:measurement(plur_su_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement_basis(plur_su_t1920, observed).
narrative_ontology:measurement(plur_su_t1929, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1929, 0.52).
narrative_ontology:measurement_basis(plur_su_t1929, observed).
narrative_ontology:measurement(plur_su_t1935, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1935, 0.55).
narrative_ontology:measurement_basis(plur_su_t1935, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The colloquial event 'the 1890 Manifesto' decomposes into three constraint stories by the causal and normative attribution of the directive's origin: this endogenous reading (revelation received by the prophet), plural_marriage_mandate__exogenous_override_reading (federal coercion forcing abandonment of a divine requirement), and plural_marriage_mandate__institutional_pragmatism_reading (strategic adaptation legitimated by the revelation narrative). All three share one epsilon referent — the post-1890 suspension arrangement — and each authors its own epsilon, beneficiary/victim structure, and claimed type over that referent; per the epsilon-invariance principle the readings are separate files linked here rather than one story with a measurement parameter. The endogenous reading is the institution's official account and the position the siblings contest: it is upstream in legitimacy (enforcement of the suspension is what pushed the exogenous reading's holders into separate organizations), while the enforcement archive and the 1891 brief that the siblings cite are evidence the endogenous account must also explain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
