% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__ishmael_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__ishmael_covenant_reading, []).

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
 *   constraint_id: abrahamic_covenant__ishmael_covenant_reading
 *   human_readable: Ishmael-Covenant Reading of the Abrahamic Promise
 *   domain: religious/comparative-theology/institutional-authority
 *
 * SUMMARY:
 *   The Abrahamic covenant functions as a persisting commitment that rival
 *   communities read differently; this story instantiates ONE reading — the
 *   ishmael_covenant_reading — as a clean, epsilon-invariant constraint. On
 *   this reading, the Genesis promise is inclusive rather than exclusive: the
 *   covenant continues through Ishmael, and the prophetic chain running from
 *   Abraham through Ishmael's line, sealed by Muhammad, validates a broader
 *   Abrahamic lineage. The standing arrangement under contest is the
 *   covenant-membership structure this reading creates: a transnational
 *   community whose boundary is drawn by acceptance of the succession claim,
 *   administered by an interpretive establishment, genuinely expansive at the
 *   entry gate, and enforced at the edges against rival succession claimants
 *   and internal dissent. The claim/metric gap is deliberate: the reading
 *   presents itself as pure inclusion (an expansion of the circle), while the
 *   authored metrics describe a structure with a real coordination function
 *   AND a real enforcement edge — the engine measures that divergence. KEY
 *   AGENTS (by structural relationship): - islamic_religious_establishment:
 *   Agenda setter (institutional/identity_locked) — administers the
 *   succession doctrine and collects its enforcement returns -
 *   sunni_muslim_majority: Primary beneficiary (organized/constrained) —
 *   inherits covenant membership and shared lineage narrative -
 *   ahmadiyya_community: Primary target (organized/identity_locked) — bears
 *   the finality doctrine's enforcement edge - bahai_post_quranic_claimants:
 *   Secondary target (powerless/trapped) — post-Quranic revelation claims
 *   rendered illegitimate - internal_reformist_dissenters: Contested internal
 *   seat (moderate/identity_locked) — pays takfir costs for pressing
 *   reinterpretation - jewish_covenant_communities: Displaced claimant
 *   (institutional/mobile) — exclusivity overridden, own tradition
 *   independently intact - converts_to_islam: Expanded-set beneficiary
 *   (moderate/constrained) — membership without bloodline -
 *   interfaith_diplomacy_initiatives: Incidental beneficiary
 *   (institutional/arbitrage) — collects convening capital from the
 *   shared-Abraham framing - academic_scholars_of_religion: Analytical
 *   observer — sees the full structure without holding a seat in it
 *   CONSTRAINT FAMILY NOTE: the colloquial label 'the Abrahamic covenant'
 *   decomposes into structurally distinct stories per the epsilon-invariance
 *   principle. This file carries the inclusive-lineage reading's own epsilon
 *   (0.42, moderate — genuine inclusion with an enforced boundary edge); the
 *   isaac_covenant_reading file carries the exclusive-channel arrangement's
 *   epsilon; the christian_supersessionist_reading file carries the
 *   faith-transfer arrangement's; the land_promise_constraint file carries
 *   the territorial axis. The contest between readings is recorded in omega
 *   variables, not folded into this constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.42).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.58).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Ishmael-Covenant Reading of the Abrahamic Promise").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious/comparative-theology/institutional-authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, '88769c33-ca84-4831-9943-58e78b55cc19').
narrative_ontology:cs_kernel_codification('88769c33-ca84-4831-9943-58e78b55cc19', fixed_text).
narrative_ontology:cs_authority_grounding('88769c33-ca84-4831-9943-58e78b55cc19', lineage).
narrative_ontology:cs_interpretation_layer_present('88769c33-ca84-4831-9943-58e78b55cc19').
narrative_ontology:cs_reading_relation('88769c33-ca84-4831-9943-58e78b55cc19', abrahamic_covenant__isaac_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('88769c33-ca84-4831-9943-58e78b55cc19', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('88769c33-ca84-4831-9943-58e78b55cc19', abrahamic_covenant__land_promise_constraint, influences).
narrative_ontology:cs_axiom('88769c33-ca84-4831-9943-58e78b55cc19', foundational, abrahamic_promise_includes_ishmael_line).
narrative_ontology:cs_axiom_status(abrahamic_promise_includes_ishmael_line, holdable).
narrative_ontology:cs_axiom_grounding('88769c33-ca84-4831-9943-58e78b55cc19', abrahamic_promise_includes_ishmael_line, theological).
narrative_ontology:cs_axiom('88769c33-ca84-4831-9943-58e78b55cc19', foundational, muhammad_seals_prophetic_succession).
narrative_ontology:cs_axiom_status(muhammad_seals_prophetic_succession, holdable).
narrative_ontology:cs_axiom_grounding('88769c33-ca84-4831-9943-58e78b55cc19', muhammad_seals_prophetic_succession, theological).
narrative_ontology:cs_reference_frame('88769c33-ca84-4831-9943-58e78b55cc19', inclusive_abrahamic_prophetic_succession).
narrative_ontology:cs_drift_state('88769c33-ca84-4831-9943-58e78b55cc19', contemporary_interfaith_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('88769c33-ca84-4831-9943-58e78b55cc19', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, sunni_muslim_majority).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_religious_establishment).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, converts_to_islam).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, interfaith_diplomacy_initiatives).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, jewish_covenant_communities).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, ahmadiyya_community).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, bahai_post_quranic_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, internal_reformist_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, jewish_covenant_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Councils of scholars, muftiates, and seminaries that articulate who inherits Abraham's promise and who speaks for the prophetic chain after Muhammad. They issue rulings on membership, train transmitters, staff state religious bureaucracies in many countries, and receive the patronage, endowments, and legal authority that attach to guardianship of the succession claim. Their institutional standing is constituted by the doctrine they administer; abandoning it would dissolve the office itself.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_religious_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Believers numbering well over a billion who receive covenant membership, a ritual calendar, marriage and burial law, and a shared ancestry narrative reaching Abraham through Ishmael. They carry ordinary obligations of creed and practice and face real social cost for leaving, but daily life under the arrangement is ordinary religious life rather than exceptional burden.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, sunni_muslim_majority, beneficiary,
    organized, generational, constrained, global).

% A renewal movement founded in nineteenth-century India whose members affirm Muhammad's prophethood while following Mirza Ghulam Ahmad as a subordinate renewer. A 1974 constitutional amendment in Pakistan declared them non-Muslim; later ordinances criminalized their self-designation; mosques have been seized or desecrated and grave markers defaced. They refuse exit — leaving the community would annihilate the identity they hold — and organize globally from a headquarters in exile.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, ahmadiyya_community, payer,
    organized, biographical, identity_locked, global).

% Communities tracing revelation to figures after Muhammad, concentrated in Iran and neighboring countries. Their founding claim — new divine revelation following the Quranic dispensation — collides directly with the finality the succession doctrine enforces, and they endure registry exclusion, property confiscation, and imprisonment in the jurisdictions where that collision is legally codified.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, bahai_post_quranic_claimants, payer,
    powerless, biographical, trapped, regional).

% Scholars, jurists, and lay thinkers who press reinterpretations of succession, authority, or law from inside the tradition. Accusations of unbelief carry career destruction, family severance, and in some jurisdictions prosecution; many operate pseudonymously or from abroad. Their professional and spiritual lives are fused with the community they contest.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, internal_reformist_dissenters, payer,
    moderate, biographical, identity_locked, global).

% Keepers of the rival exclusivity reading. The inclusive rereading operates on their ancestral text, asserting that the promise they channel was always wider than their tradition holds. They lose the uncontested standing of their exclusivity claim wherever the broader reading circulates, yet their own covenant life proceeds intact and independent — they can decline the entire framing at no structural cost, and interfaith settings often restore them to honored senior-branch standing.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, jewish_covenant_communities, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__ishmael_covenant_reading, jewish_covenant_communities, beneficiary).

% People with no descent claim who acquire full Abrahamic membership through creed alone — the inclusive reading is precisely what makes membership available to them without bloodline. They gain belonging and a genealogy of meaning, and they bind themselves to the community's obligations and to its boundary disputes as well.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, converts_to_islam, beneficiary,
    moderate, biographical, constrained, global).

% Dialogue bodies, joint declarations, and shared-heritage projects that trade on the common-Abraham framing the inclusive reading supplies. They collect convening power and diplomatic capital from the expanded-family narrative and can shift framing or withdraw without material loss.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, interfaith_diplomacy_initiatives, beneficiary,
    institutional, generational, arbitrage, global).

% Historians and comparativists who map the contest over the covenant's channel without holding a seat in it. They produce the reception histories, philologies, and institutional analyses that the other seats cite or ignore.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, academic_scholars_of_religion, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__ishmael_covenant_reading, islamic_religious_establishment).
narrative_ontology:fixing_cost_class(abrahamic_covenant__ishmael_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a transnational community on the order of two billion people around shared Abrahamic monotheistic identity, practice, law, and calendar by fixing the covenant's transmission channel through Ishmael and validating it via prophetic succession sealed by Muhammad. It solves the membership-boundary problem — who inherits Abraham's promise — through creed and succession rather than bloodline-exclusive election, which is what makes the entry gate open.
% TRANSFER_FUNCTION: Moves recognition and legitimacy: confers covenant-membership status on those accepting the succession claim; moves doctrinal authority, boundary-setting power, and the patronage attached to guardianship toward the interpretive establishment; extracts creedal and legal conformity from members; and exacts exclusion from those advancing rival succession claims or rival exclusivity claims over the same promise.
% ABSENT_VOICES: Heterodox claimant communities are present as litigants but absent from the councils that define orthodoxy — they would object that a finality enforced by statute excludes renewal claims the tradition's own founding logic otherwise contemplates. Jewish covenant communities are absent from the rereading of their own scripture. Internal dissenting voices face accusation costs that chill their participation in the very forums where the boundary is drawn.
% DISAPPEARANCE_RATIONALE: If the succession claim and its enforcement vanished overnight, the self-understanding of the largest Muslim communities would lose its lineage anchor, the establishment's boundary-setting authority would evaporate, the excluded claimant communities would immediately re-enter the category they are barred from, interfaith 'shared Abrahamic heritage' diplomacy would lose its framing, and the downstream territorial contest would lose one of its legitimating inputs — the arrangement of covenant membership across a substantial fraction of humanity would have to be renegotiated from scratch.
% FOUNDING_PROBLEM: After Abraham, who carries the promise — and by what warrant? The reading was articulated to answer the seventh-century Arabian community's claim to Abrahamic inheritance: to root the new community's legitimacy in the oldest monotheistic lineage, to read the Genesis promise as having always included Ishmael's line, and to solve membership by prophetic succession rather than by bloodline-exclusive election.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic scholars of religion publish the genealogy of the dispute as unresolved; Jewish interlocutors engage and dispute the claim in documented interfaith literature — the persistence of the dispute itself attests liveness; and court records in enforcing jurisdictions (the evidentiary proceedings behind Pakistan's 1974 constitutional amendment, litigation over Ahmadi self-designation) show the question of who counts as covenant-heir being adjudicated as a live matter. No neutral arbiter exists, and none is claimed; the corroboration is that hostile, indifferent, and friendly parties alike continue treating the question as open.
narrative_ontology:disappearance_verdict(abrahamic_covenant__ishmael_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__ishmael_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__ishmael_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__ishmael_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__ishmael_covenant_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__ishmael_covenant_reading_tests).
:- end_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end): the entry gate is genuinely open — membership by creed rather than bloodline is the reading's signature achievement — but the succession-validation mechanism doubles as an exclusion mechanism, and the establishment collects authority rents from administering the boundary. Suppression (0.58) is a raw structural property, unscaled by power or scope: it reflects the enforcement machinery attached to the finality doctrine — constitutional exclusion of a named community in Pakistan, blasphemy statutes criminalizing self-designation, imprisonment and property confiscation of post-Quranic claimants in Iran, and social takfir norms that operate where statute does not. The mechanism is predominantly structural (statute and state machinery) with a secondary internalized layer (community-level accusation norms that persist in non-enforcing jurisdictions). Theater ratio is low (0.12): the identity-coordination function is really performed — calendars, rites, law, education all run through it — with only a thin ceremonial layer of reaffirmation. Accessibility collapse is moderate (0.48): rival covenant readings remain fully live for other communities, exit from the community is possible though socially costly, and the framing itself competes openly in interfaith space. Resistance is moderate (0.52): heterodox communities resist exclusion through litigation and international advocacy, reformists contest from inside, and rival traditions reject the rereading — while the overwhelming majority assents. The measurement series run on one shared time grid (points 0-60 indexing consolidated phases: classical consolidation, imperial maturity, colonial-era disruption, the late-modern enforcement hardening around the Ahmadi controversy and subsequent statute-making, and the contemporary period); every tracked metric is authored at every point, and end-state values match the base_properties scalars. Claim and metrics are independent authored facts: tangled_rope is claimed from the structure (genuine coordination plus asymmetric extraction plus active enforcement), not tuned to any predicted output.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the establishment's position the arrangement is the coordination structure it built, transmits, and legitimately guards — its institutional identity has fused with the doctrine ('the office IS the guardianship'), so exit is unthinkable without self-dissolution; that seat computes near pure coordination. From the target seats the same structure operates as enforced exclusion: an Ahmadi or Baha'i experiences the succession claim primarily as the thing that renders them outlaw, and their identity lock cuts the other way — they cannot exit without annihilating the self they hold. The majority seat is genuinely mixed: ordinary religious life subsidized by membership, with diffuse exposure to the boundary disputes. The victim seats cannot coalition: Ahmadis, Baha'is, reformists, and Jewish communities are mutually theologically hostile and structurally dispersed, which is precisely why the enforcement edge holds at low cost. If the identity frame broke — if finality softened into unenforced doctrine across jurisdictions — the target seats' effective burden would collapse and the arrangement would drift toward the coordination-only reading of itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The establishment sits near the beneficiary end: it collects the arrangement's returns (authority, patronage, boundary-setting power) and pays only maintenance costs. The majority and converts sit low-to-symmetric: broad real benefits, ordinary obligations. The targets sit near the full-target end: Ahmadis (identity_locked) and Baha'i claimants (trapped) bear the enforcement edge with no exit that preserves identity or safety; internal reformists (identity_locked) pay career and kinship costs. Jewish covenant communities bear a real but diffuse cost — the displacement of their exclusivity claim — yet their mobile exit (their tradition runs intact independently of this arrangement) dampens their directionality below the trapped targets'. Interfaith initiatives hold arbitrage-grade exit and collect incidental gains, placing them nearest the beneficiary end despite holding no doctrinal seat. Scope is global for most seats, which the engine weighs in scaling effective extraction; the enforcement edge concentrates regionally where statute codifies it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — who carries Abraham's promise, and by what warrant — remains live: it structures interfaith contest, internal heterodoxy disputes, and resonates into the territorial contest downstream. Nothing here is vestigial; the arrangement performs its function daily at planetary scale, so no mandatrophy resolution is declared and the founding-problem status (live) matches the disappearance verdict (world_rearranges) with no mismatch flag. The classification discipline matters symmetrically here: an apologetic reading would label the arrangement pure coordination (rope) on the strength of its open entry gate, erasing the Ahmadis and Baha'is; a polemical reading would label it pure extraction (snare), erasing the genuine, heavily used membership coordination that roughly two billion people's religious lives run through. Tangled rope holds both facts: the same structure that includes the convert excludes the rival claimant, and the enforcement that protects the succession claim is the enforcement that prosecutes it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the abrahamic_covenant kernel — the ishmael_covenant_reading. What structurally changes if a sibling reading is adopted instead?',
    'Cross-file comparison of the linked sibling stories: compare beneficiary sets, victim sets, and epsilon under each reading of the same kernel.',
    'Adopting isaac_covenant_reading collapses the beneficiary set to Isaac-line communities and converts Ishmael''s descendants into the excluded party; adopting christian_supersessionist_reading transfers covenant-bearer status to the Church and dissolves the lineage-succession mechanism entirely; resolving land_promise_constraint one way or another overlays a territorial axis this reading does not itself fix.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel reading this file instantiates and what sibling adoption would change.').

omega_variable(
    genesis_text_underdetermination,
    'Where is the disagreement located: does Genesis 17''s promise language itself determine the covenant''s channel, or is the text underdetermined such that the reading is carried by extra-textual authority (Qur''anic retelling versus rabbinic limitation)?',
    'Philological analysis of the promise clauses alongside the two reception histories, identifying which interpretive moves each reading requires the text to support.',
    'If the text underdetermines the channel, the contest is between interpretive authorities and the constraint''s extraction concentrates in enforcing one authority''s claim; if the text determines it, one reading carries textual warrant and the other carries pure institutional interest — changing which seats compute as coordinated versus coerced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genesis_text_underdetermination, conceptual, 'Whether the kernel text fixes the covenant channel or the readings import it.').

omega_variable(
    enforcement_edge_jurisdictional_contingency,
    'How much of the measured suppression belongs to the finality doctrine as such, versus to state-level arrangements that weaponize it?',
    'Jurisdictional comparison: states holding identical doctrine with no enforcement machinery (most of the Muslim world) versus the few that codify it (Pakistan''s 1974 constitutional amendment and ordinance-era statutes; Iran''s treatment of Baha''i institutions).',
    'If enforcement is jurisdictionally contingent, the reading''s own epsilon is materially lower than measured and the excess belongs to separate state-level stories; if doctrinally intrinsic, the enforcement edge travels with the reading and the tangled-rope weighting stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_edge_jurisdictional_contingency, empirical, 'Whether the suppression edge is intrinsic to the reading or borrowed from state machinery.').

omega_variable(
    inclusion_vs_appropriation_reception,
    'Is the inclusive rereading received by the rival tradition as honored extension or as dispossession of its scripture — and does the coordination function run through shared acknowledgment or unilateral claim?',
    'Reception studies across documented interfaith exchanges: where Jewish interlocutors endorse shared-Abraham framings versus where they record the rereading as supersession-in-reverse.',
    'If reception is substantially shared, the coordination function is two-sided and the arrangement sits nearer pure coordination; if unilateral, the expansion extracts narrative control from the rival seat and the extraction weighting rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inclusion_vs_appropriation_reception, preference, 'Whether the inclusive move coordinates jointly or appropriates unilaterally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ishmael_covenant_reading_tr_t0, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t10, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t20, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t30, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t40, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t50, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(ishmael_covenant_reading_tr_t60, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 60, 0.12).

% Extraction over time
narrative_ontology:measurement(ishmael_covenant_reading_be_t0, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ishmael_covenant_reading_be_t10, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(ishmael_covenant_reading_be_t20, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(ishmael_covenant_reading_be_t30, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(ishmael_covenant_reading_be_t40, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(ishmael_covenant_reading_be_t50, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(ishmael_covenant_reading_be_t60, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ishmael_covenant_reading_su_t0, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(ishmael_covenant_reading_su_t10, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(ishmael_covenant_reading_su_t20, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(ishmael_covenant_reading_su_t30, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(ishmael_covenant_reading_su_t40, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(ishmael_covenant_reading_su_t50, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(ishmael_covenant_reading_su_t60, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__ishmael_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Abrahamic covenant' decomposes into structurally distinct constraints: two lineage-channel readings (isaac_covenant_reading, exclusive; this file, inclusive), one mode-of-transfer reading (christian_supersessionist_reading, covenant borne by faith rather than lineage), and one territorial-grant constraint (land_promise_constraint). Each carries its own epsilon, beneficiaries, and victims. The lineage readings sit upstream of the land-promise contest because the channel question determines whose territorial claim a grant would vindicate; this file therefore links all three siblings per the family rule, and the upstream inclusive/exclusive split exerts structural pressure on the downstream territorial dispute without resolving it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
