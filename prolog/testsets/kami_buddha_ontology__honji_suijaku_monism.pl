% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji-Suijaku Monism: Kami as Buddhist Manifestations
 *   domain: religious/philosophical
 *
 * SUMMARY:
 *   The honji-suijaku (original-ground / phenomenal-trace) reading of
 *   shinbutsu-shugo ('kami-buddha fusion') asserts that kami are
 *   ontologically identical to Buddhas and bodhisattvas—specifically, kami
 *   are phenomenal manifestations (suijaku) grounded in Buddhist metaphysical
 *   ultimates (honji). This reading was institutionalized by Buddhist
 *   authorities, literati philosophers, and state-aligned religious policy
 *   from the early medieval through early modern periods. It benefits
 *   Buddhist institutions by subordinating kami-understanding to Buddhist
 *   interpretive authority. It extracts from kami practitioners and folk
 *   communities by redefining their direct kami-relationships as relations to
 *   Buddhist manifestations, requiring reinterpretation of their own
 *   practices through a framework they did not produce. The constraint is
 *   CLAIMED as a tangled_rope (coordination function: unified metaphysical
 *   framework; victims: kami practitioners and folk communities who pay
 *   through loss of interpretive authority). The authored metrics reflect the
 *   tension between the genuine coordination it provides (philosophical
 *   coherence, practical ritual integration) and the extraction it performs
 *   (asymmetric reinterpretation, institutional authority consolidation).
 *
 * KEY AGENTS:
 *   - Buddhist institutional authority: primary agenda-setter, benefits from subordination of kami to Buddhist hierarchy
 *   - Literati philosophers: secondary beneficiary, gains prestige from systematizing apparent contradiction into philosophical unity
 *   - Kami practitioners (powerless, identity-locked): primary victims, experience reinterpretation of their direct kami-relationships as manifestations
 *   - Folk religious communities (powerless, constrained exit): victims, lose authority over their own kami narratives and shrine practices
 *   - Shinto reformist elite (excluded, organized): would-be objectors, barred from authoritative voice during peak constraint period
 *   - Ritual specialists (moderate power): pragmatic operators of the constraint, know empirically it diverges from practice but cannot adjudicate doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.58).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.71).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji-Suijaku Monism: Kami as Buddhist Manifestations").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious/philosophical").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, '5bec1d03-af85-46f1-87c0-5cdf542ff4e7').
narrative_ontology:cs_kernel_codification('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', distributed).
narrative_ontology:cs_authority_grounding('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', extraction).
narrative_ontology:cs_interpretation_layer_present('5bec1d03-af85-46f1-87c0-5cdf542ff4e7').
narrative_ontology:cs_reading_relation('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', foundational, single_ultimate_reality_foundation).
narrative_ontology:cs_axiom_status(single_ultimate_reality_foundation, holdable).
narrative_ontology:cs_axiom_grounding('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', single_ultimate_reality_foundation, deontological).
narrative_ontology:cs_axiom('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', foundational, kami_derivative_manifestation_status).
narrative_ontology:cs_axiom_status(kami_derivative_manifestation_status, holdable).
narrative_ontology:cs_axiom_grounding('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', kami_derivative_manifestation_status, deontological).
narrative_ontology:cs_reference_frame('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', buddhist_metaphysical_monism).
narrative_ontology:cs_drift_state('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', early_modern_shinto_reformism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5bec1d03-af85-46f1-87c0-5cdf542ff4e7', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutional_authority).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, literati_philosophers).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, kami_practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, folk_religious_communities).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddhist_metaphysical_universality).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, hierarchical_reality_structure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the honji-suijaku reading through scriptural interpretation, temple practice, and doctrinal pronouncement. Benefits from the reading because it subordinates kami to Buddhist ontological hierarchy, positioning Buddhist institutions as the authoritative interpreters of all sacred reality—including phenomena practitioners understood as independent kami. Collects institutional prestige, ritual authority, and the ability to adjudicate what counts as legitimate religious practice.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Build philosophical and textual apparatus to demonstrate that kami are coherent only as manifestations of Buddhist bodhisattvas. Benefits from the systematization the reading provides: a unified metaphysical framework that explains coexistence through hierarchy rather than allowing incoherent plurality. Gains intellectual authority and prestige by solving the puzzle of how to integrate incompatible sacred systems.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, literati_philosophers, beneficiary,
    institutional, generational, mobile, national).

% Experience kami as direct, independent sacred presences in landscape, community, and life-world. Under the honji-suijaku reading, their direct relationship to kami is reconceptualized as relation to Buddhist manifestations, subordinating their understanding to an interpretive framework they did not produce. Constrained by institutional enforcement and by cultural pressure to accept literati interpretation as authoritative. Cannot exit without abandoning the kami-relationship that constitutes their identity and community practice.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kami_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Maintain shrine practices, seasonal kami festivals, protective kami invocations, and local kami-centered ritual that treated kami as autonomous agents. The honji-suijaku reading subordinates these practices to Buddhist interpretation: shrine renovations adopt Buddhist iconography, ritual language is reinterpreted through Buddhist doctrine, local kami narratives are subsumed into Buddhist salvation narratives. Community cohesion is maintained through practice, but the authority to adjudicate meaning has shifted to institutions outside the community.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, folk_religious_communities, payer,
    powerless, biographical, constrained, local).

% Emerge (historically, in early modernity) to contest the honji-suijaku reading by asserting kami as ontologically prior, independent, or even superior to Buddhist deities. Would argue that the constraint reverses the true hierarchy and imposes Buddhist metaphysics on indigenous sacred categories. Structurally barred from authoritative voice during the peak constraint period; their objections are not admitted as legitimate alternatives in the official discourse controlled by Buddhist institutions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shinto_reformist_elite, excluded,
    organized, generational, constrained, national).

% Manage the practical enactment of syncretism through dual-tradition ritual: Buddhist ceremonies at temples, kami invocations at shrines, overlapping cosmologies in practice. Navigate the constraint by performing it without necessarily endorsing the honji-suijaku reading fully—they know empirically that kami and Buddhas are treated differently in practice even though doctrinal unity is asserted. Their pragmatic knowledge of the gap between doctrine and practice could undermine the constraint if articulated publicly.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, ritual_specialists, observer,
    moderate, biographical, constrained, regional).

% External scholarly position examining the constraint's structure and persistence. Neither collecting from nor bearing the cost of the constraint; observes the asymmetries it produces and the work required to maintain it against practical and conceptual pressure.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutional_authority).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the practical coexistence of kami and Buddhist deities by providing a single metaphysical framework—kami as suijaku (phenomenal manifestations) grounded in Buddhist bodhisattvas (honji, original ground)—that allows temples and shrines, Buddhist and kami rituals, and dual practice to proceed without requiring explicit choice between them. Eliminates the need to admit two ultimate realities or two separate domains.
% TRANSFER_FUNCTION: Transfers interpretive authority and doctrinal priority from kami practitioners and folk communities to Buddhist institutional authority and literati philosophers. Moves kami from primary to secondary ontological status (manifestation rather than independent entity), subordinating the cosmologies practitioners generated from experience to a philosophical framework produced outside their communities.
% ABSENT_VOICES: Kami practitioners and folk communities experience the constraint as imposed doctrine rather than lived discovery; their voices are excluded from the authoritative discourse that declares kami and Buddhas ontologically identical. Shinto reformist elites, emerging later, attempt to reverse the hierarchy but are kept out of the constraint's definition-setting authority by its institutional enforcement. Ritual specialists know empirically that the constraint does not match practice but are not invited to adjudicate doctrine.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku reading and its enforcement vanished, kami would revert to perceived autonomy in practice; Buddhist and kami ritual would bifurcate into more clearly separated domains; the interpretive authority of Buddhist institutions over kami-understanding would collapse; folk communities would reclaim authority over their own kami narratives; the incentive to systematize and hierarchize would diminish. The religious landscape would reorganize into plurality rather than unity-under-hierarchy.
% FOUNDING_PROBLEM: Early medieval Japan experienced the simultaneous existence of kami (indigenous sacred presences organized in Shinto cosmology) and Buddhas (Buddhist deities and metaphysical principles introduced through Buddhism). The practical coexistence worked at the level of ritual and shrine organization but created an intellectual problem: how can two ultimate sacred realities exist in one universe without contradiction? The honji-suijaku reading solved this by denying the problem's premise—kami are not a second ultimate reality but manifestations of the Buddhist ultimate.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutional authorities attest the founding problem is solved by the honji-suijaku reading and remains solved; the framework prevents logical contradiction and allows harmonious dual practice. Kami practitioners and folk communities attest the problem was never theirs—they experienced kami and Buddhas as easily coexistent at the practical level; the 'problem' was an intellectual construction of literati seeking systematization. Shinto reformists attest the founding problem has been misdefined—the real problem is the Buddhist subordination of kami, not logical incompatibility. Modern scholars of Japanese religion outside the benefiting traditions document that the constraint was a solution to an elite intellectual problem, not a solution to a problem practitioners actually faced.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaks at t=30 (0.61) during the period of maximum institutional enforcement and literati systematization (roughly 16th-18th centuries in Japanese history), then stabilizes at 0.58 in later periods as the constraint's authority weakens with modernization and Shinto reformism becomes intellectually viable. Theater ratio rises steadily from 0.35 to 0.64 between t=0 and t=30, indicating that an increasing proportion of the constraint's persistence becomes performative—the institutional assertion of unity exceeds the practical coherence it produces. As kami practitioners and folk communities increasingly act in ways that treat kami and Buddhas as functionally distinct (despite doctrinal unity), the constraint requires more active maintenance to appear natural. Suppression requirement similarly rises and plateaus, reflecting the steady institutional work required to suppress kami-autonomy framing and alternative interpretations. The shared time grid aligns all three series; each point represents an authored judgment about the constraint's state at that historical moment, not a smooth projection.
 *
 * PERSPECTIVAL GAP:
 *   Buddhist institutional authority and literati philosophers experience the constraint as the achievement of philosophical coherence and the resolution of an intellectual problem—a genuine coordination accomplishment. From their seat, the honji-suijaku reading is a discovery about reality's true structure, not an imposition. Kami practitioners and folk communities experience the same constraint as external reinterpretation that subordinates their understanding of direct kami-relationships to a framework produced by institutions outside their communities. From their seat, the constraint is not a coordination achievement but an extraction masked as philosophical discovery. Ritual specialists occupy a third position: they know both frameworks work practically and can switch between them depending on context, giving them a degree of freedom the constraint attempts to foreclose through the assertion that only one ontological truth exists. The engine should compute these seats' directionality values as significantly divergent.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional authority derives d near 0.0 (full beneficiary): they control the constraint's definition, benefit from institutional prestige and interpretive authority, and have high exit options (can shift doctrine if needed, can ally with competing institutional powers). Literati philosophers derive d around 0.15 (strong beneficiary): they build careers on the systematization the constraint provides and benefit from the intellectual authority it grants, but are somewhat constrained by the need for the framework to maintain institutional support. Kami practitioners derive d near 0.95 (near-full target): they are powerless, identity-locked (their identity constitutes itself through kami-relationship), and experience subordination of their understanding; exit means abandoning the relationships that constitute their identity. Folk religious communities derive d around 0.85 (strong target): they are powerless, have constrained exit (leaving means community dissolution), and bear the cost of reinterpretation of their ritual and narrative frameworks. Shinto reformist elite derive d around 0.7 (strong target): they are excluded from the authority to define the constraint and bear the cost of being positioned as heretical alternatives; their power (organized) gives them somewhat higher d than the powerless, but they are trapped outside the system's authority structure. Ritual specialists derive d around 0.45 (near-symmetric): they benefit from practical coherence the constraint provides but pay by living in the gap between what doctrine asserts and what practice enacts, carrying the cognitive and social cost of managing that gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The honji-suijaku reading avoids straightforward mandatrophy because the founding problem (intellectual coherence of two sacred systems) remains perpetually contentious rather than solved. However, the constraint exhibits weak-mandatrophy characteristics: the coordination problem the reading was designed to solve—'how to think about kami and Buddhas without logical contradiction'—is genuinely solved by the framework. But the constraint's persistence also depends on the extraction it performs: suppressing alternative readings (domain-partition, kami-autonomy), subordinating folk interpretive authority, and maintaining institutional dominance over religious meaning-making. The constraint is not transparently a solution to a shared problem; it is a solution to a problem formulated by one party (literati seeking systematization) imposed on parties who did not share it. The theater ratio rising to 0.64 indicates that in later periods the constraint's primary function shifts from coordination toward performance of unity—institutional assertion that one ontological truth exists becomes the main work, while the coordination benefit (practical ritual integration) remains constant. This is the signature of a constraint whose mandate has partially atrophied: it persists not because it solves the founding problem better than alternatives, but because institutions benefit from its persistence and have the power to enforce it. The constraint avoids full piton status because the coordination function remains real and useful; but it is drifting toward piton as the theater ratio rises and the founding problem remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systematicity_vs_lived_pluralism,
    'Is the honji-suijaku monism a discovery of underlying Buddhist metaphysical truth, or a systematization imposed on a religious landscape that functions perfectly well as irreducible plurality?',
    'Ethnographic comparison across societies where systematic monism was not enforced: do kami and buddhas naturally produce the same kind of coherence in practice without the honji-suijaku framework? Does the framework solve a problem practitioners actually face, or a problem literati invented?',
    'If the framework is imposed systematization, the constraint reclassifies from tangled_rope toward snare (pure extraction of interpretive authority masked as coordination). If kami and buddhas naturally cohere without it, the coordination function is illusory and the framework is primarily extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systematicity_vs_lived_pluralism, empirical, 'Whether honji-suijaku systematization solves a genuine coordination problem or imposes philosophical order on working plurality.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) achieved through institutional coercion (external barriers, textual authority, exclusion from discourse), or through internalized acceptance of the framework by kami practitioners themselves?',
    'Post-enforcement suppression trajectory: during periods when institutional enforcement weakens (Shinto reformism, modernization), does suppression persist in practitioner behavior and belief, or does alternative framing reemerge? If it persists, suppression is partially internalized; if it reverses, suppression was structural.',
    'If internalized, the constraint''s effective suppression on practitioners is higher than the structural measure suggests—the framework persists in practitioner consciousness even when institutional enforcement relaxes. If structural, practitioners would shift toward kami-autonomy framing as soon as institutional pressure eases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of kami-autonomy framing is structural (institutional coercion) or internalized (accepted as metaphysical truth).').

omega_variable(
    alternative_reading_foreclosure,
    'Does the honji-suijaku monism logically foreclose the domain-partition reading (kami and buddhas as distinct but coordinated domains), or merely suppress it institutionally?',
    'Logical analysis: can both readings be true within a single metaphysical framework, or does honji-suijaku monism''s core premise (single ultimate ground) directly contradict domain-partition''s core premise (ontologically distinct entities)? If they can coexist logically (e.g., one ultimate with two manifestation modes), the readings coexist; if they genuinely contradict, honji-suijaku forecloses domain-partition.',
    'If domain-partition is foreclosed, it is eliminated as a philosophical option and its persistence becomes purely institutional resistance (snare-side). If domain-partition remains logically viable, the honji-suijaku reading''s dominance is purely institutional and not metaphysically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether honji-suijaku monism logically forecloses the domain-partition reading or merely suppresses it institutionally.').

omega_variable(
    identity_lock_mechanism_kami_relationship,
    'What specific identity-fusion mechanism binds kami practitioners to the kami-relationship such that exit becomes identity-dissolution?',
    'Ethnographic and historical analysis of kami-practitioner identity constitution: is identity fused with kami-relationship through kinship/lineage (hereditary shrine priesthood), relational identity (self-constituted through devotional relationship to specific kami), ideological identity (worldview making kami-autonomy self-evident), or institutional identity (practitioner role defined within shrine hierarchy)?',
    'Different mechanisms require different approaches to constraint relaxation. Lineage-based fusion persists across institutional change (exit costs remain high). Relational fusion might reorient if kami-relationship is reframed (honji-suijaku framework itself becomes the lock). Ideological fusion breaks when counterdoctrines become intellectually viable. Institutional fusion dissolves when institution changes. Identifying the mechanism clarifies how the identity-lock could be broken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_kami_relationship, empirical, 'Mechanism of identity-fusion binding kami practitioners to kami-relationship.').

omega_variable(
    kernel_reading_committer_frame,
    'Is this constraint genuinely one coherent reading of a single kernel, or does the honji-suijaku framework itself instantiate multiple incompatible framings bundled together?',
    'Internal coherence analysis: does the honji-suijaku framework consistently treat kami as derivative manifestations throughout its application, or does it sometimes treat kami as autonomous agents, sometimes as pure Buddhist emanations, sometimes as coequal partners, depending on context? If internally inconsistent, it may be the ''incoherent_bundle'' reading itself rather than a distinct reading.',
    'If internally incoherent, the constraint classification shifts toward piton or snare (sustained by performance and institutional force rather than internal coherence). If coherent, the framework is a genuine alternative reading and should be evaluated against domain-partition and incoherent-bundle on their own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether honji-suijaku monism is internally coherent or itself an institutionally sustained bundle of contradictions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(kami_tr_t0, projected).
narrative_ontology:measurement(kami_tr_t10, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(kami_tr_t10, observed).
narrative_ontology:measurement(kami_tr_t20, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(kami_tr_t20, observed).
narrative_ontology:measurement(kami_tr_t30, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 30, 0.64).
narrative_ontology:measurement_basis(kami_tr_t30, observed).
narrative_ontology:measurement(kami_tr_t40, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 40, 0.65).
narrative_ontology:measurement_basis(kami_tr_t40, observed).
narrative_ontology:measurement(kami_tr_t50, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 50, 0.62).
narrative_ontology:measurement_basis(kami_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(kami_be_t0, projected).
narrative_ontology:measurement(kami_be_t10, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(kami_be_t10, observed).
narrative_ontology:measurement(kami_be_t20, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(kami_be_t20, observed).
narrative_ontology:measurement(kami_be_t30, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(kami_be_t30, observed).
narrative_ontology:measurement(kami_be_t40, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(kami_be_t40, observed).
narrative_ontology:measurement(kami_be_t50, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(kami_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(kami_su_t0, projected).
narrative_ontology:measurement(kami_su_t10, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(kami_su_t10, observed).
narrative_ontology:measurement(kami_su_t20, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(kami_su_t20, observed).
narrative_ontology:measurement(kami_su_t30, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 30, 0.73).
narrative_ontology:measurement_basis(kami_su_t30, observed).
narrative_ontology:measurement(kami_su_t40, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(kami_su_t40, observed).
narrative_ontology:measurement(kami_su_t50, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(kami_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__honji_suijaku_monism, 0.12).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, shinbutsu_shugo_institutional_practice).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, shinto_reformism_vernacular_resistance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kami-buddha ontology kernel. The honji-suijaku monism reading asserts single ultimate reality with kami as Buddhist manifestations (hierarchical, systematized). It coexists with domain-partition (separate but coordinated realms) and incoherent-bundle (institutionalized contradiction). All three readings are constraints on the same problem space; each instantiates different ε and structural beneficiary/victim arrangements. This story formalizes the honji-suijaku reading; sibling readings are separate constraint stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
