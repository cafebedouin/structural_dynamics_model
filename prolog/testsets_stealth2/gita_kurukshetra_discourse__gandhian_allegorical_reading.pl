% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__gandhian_allegorical_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Kurukshetra Discourse — Gandhian Allegorical Reading (Internal Struggle, Ahimsa Supreme)
 *   domain: religious/hermeneutic/ethical
 *
 * SUMMARY:
 *   The kernel is the Bhagavad Gita's Kurukshetra discourse as a persisting
 *   interpretive-moral authority: a divine command, delivered on the edge of
 *   a civil war, that resolves a warrior's refusal to fight. This story
 *   instantiates the Gandhian allegorical reading of that kernel. Per the
 *   kernel-reading epsilon rule, epsilon's referent is the standing
 *   arrangement under contest — the discourse's orthodox deployment as a
 *   mandate of caste duty and legitimation of righteous violence — assessed
 *   by this reading's own lights: a structure that overrides conscience by
 *   command, converts birth into obligation, sanctifies fratricide, and
 *   reserves meaning to an interpretive monopoly. The reading's structural
 *   delta: caste hierarchy exits the constraint set (no divine mandate
 *   survives scrutiny), physical violence is repudiated, interpretive
 *   authority shifts from Brahminical office to individual moral conscience,
 *   ahimsa is elevated as supreme principle, and the victim set is renamed —
 *   those subjected to caste's structural violence and to literal war. Claim
 *   and metrics are independent authored facts: claimed_type is tangled_rope
 *   from structural judgment (the arrangement genuinely coordinates
 *   role-order and meaning AND asymmetrically harms identifiable seats, under
 *   active enforcement), while the metrics describe the arrangement's
 *   operation as this reading assesses it. Where the engine's per-seat
 *   computation diverges from either, that divergence is the datum the corpus
 *   exists to take.
 *
 * KEY AGENTS:
 *   - brahminical_interpretive_class — agenda-setter and principal beneficiary (institutional / identity_locked): administers the interpretive machinery that renders the text as caste duty and war mandate; collects interpretive deference and social precedence
 *   - kshatriya_warrior_aristocracy — beneficiary with secondary exposure (powerful / constrained): receives sanctified rule; supplies the bodies its own legitimation conscripts
 *   - caste_bound_warrior_conscripts — primary target (moderate / trapped): bound by birth-duty to kill kin under sanction of disgrace; the Arjuna seat generalized
 *   - shudra_and_outcaste_castes — primary target (powerless / trapped): bear the structural violence of the role order the divine warrant secures
 *   - conscience_objectors_to_war — target (moderate / constrained): moral refusal answered by command rather than argument
 *   - gandhian_allegorical_adherents — excluded voice (organized / mobile): contest the orthodox deployment from outside the interpretive monopoly; built parallel institutions rather than exiting
 *   - comparative_hermeneutic_scholars — analytical observer (analytical / analytical): sees the full structure across all three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.8).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.78).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Kurukshetra Discourse — Gandhian Allegorical Reading (Internal Struggle, Ahimsa Supreme)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious/hermeneutic/ethical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__gandhian_allegorical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '087e3390-f2fb-45e6-bbc8-eb7daf94162d').
narrative_ontology:cs_kernel_codification('087e3390-f2fb-45e6-bbc8-eb7daf94162d', fixed_text).
narrative_ontology:cs_authority_grounding('087e3390-f2fb-45e6-bbc8-eb7daf94162d', practice).
narrative_ontology:cs_reading_relation('087e3390-f2fb-45e6-bbc8-eb7daf94162d', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('087e3390-f2fb-45e6-bbc8-eb7daf94162d', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('087e3390-f2fb-45e6-bbc8-eb7daf94162d', foundational, ahimsa_supreme_hermeneutic_principle).
narrative_ontology:cs_axiom_status(ahimsa_supreme_hermeneutic_principle, holdable).
narrative_ontology:cs_axiom_grounding('087e3390-f2fb-45e6-bbc8-eb7daf94162d', ahimsa_supreme_hermeneutic_principle, deontological).
narrative_ontology:cs_axiom('087e3390-f2fb-45e6-bbc8-eb7daf94162d', foundational, kurukshetra_internal_struggle_metaphor).
narrative_ontology:cs_axiom_status(kurukshetra_internal_struggle_metaphor, holdable).
narrative_ontology:cs_axiom_grounding('087e3390-f2fb-45e6-bbc8-eb7daf94162d', kurukshetra_internal_struggle_metaphor, theological).
narrative_ontology:cs_axiom('087e3390-f2fb-45e6-bbc8-eb7daf94162d', secondary, individual_conscience_interpretive_authority).
narrative_ontology:cs_axiom_status(individual_conscience_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('087e3390-f2fb-45e6-bbc8-eb7daf94162d', individual_conscience_interpretive_authority, deontological).
narrative_ontology:cs_reference_frame('087e3390-f2fb-45e6-bbc8-eb7daf94162d', gita_as_anasakti_ahimsa_gospel).
narrative_ontology:cs_drift_state('087e3390-f2fb-45e6-bbc8-eb7daf94162d', contemporary_hindutva_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('087e3390-f2fb-45e6-bbc8-eb7daf94162d', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpretive_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, kshatriya_warrior_aristocracy).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_bound_warrior_conscripts).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, shudra_and_outcaste_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, conscience_objectors_to_war).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, kshatriya_warrior_aristocracy).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_supremacy_doctrine).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the interpretive structure through which the discourse reaches adherents: decides what the text commands, trains its transmitters, and adjudicates disputes over duty. Receives interpretive deference, ritual precedence, and social supremacy that flow from holding the monopoly on meaning. Its members' identity is fused with the office — one who renounces the interpretive role loses standing, livelihood, and self-concept together. Exit historically meant descent into lay status or renunciation, both social deaths for the class's members.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpretive_class, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpretive_class, beneficiary).

% Receives sanctified rule: the discourse converts its war-making into religious obligation and its hierarchy into divine order. Its members are also the discourse's conscription targets — the paradigm crisis, a prince ordered to kill his kin and teachers, is an aristocrat's crisis — so the seat both collects the legitimation and supplies the bodies. Leaving the warrior role meant disgrace and loss of the estate that role secured.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, kshatriya_warrior_aristocracy, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, kshatriya_warrior_aristocracy, payer).

% Individual soldiers bound to fight by birth-duty rather than choice. The discourse resolves their refusal by command: their moral objection is answered with divine authority and the sanction of disgrace. Refusal meant social death for them and their lines; compliance meant killing kin. Exit was effectively closed on both sides.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_bound_warrior_conscripts, payer,
    moderate, biographical, trapped, continental).

% Bear the structural costs of the role order the discourse's divine warrant secures: hereditary service obligations, exclusion from sacred knowledge and ritual, and for the outcaste, physical and social untouchability. Position is inherited; exit historically required conversion at catastrophic cost — loss of community, livelihood, and legal standing. Their objections had no seat in the interpretive conversation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, shudra_and_outcaste_castes, payer,
    powerless, generational, trapped, continental).

% Individuals whose moral judgment refuses the violence the discourse commands — the Arjuna position generalized. They hold no office and no monopoly; their refusal is met not with argument but with command backed by divine authority and social sanction. Some exit through renunciation; most absorb the override and comply.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, conscience_objectors_to_war, payer,
    moderate, biographical, constrained, regional).

% Read the discourse as an allegory of inner struggle and organize life and politics around nonviolence. They stand outside the orthodox interpretive monopoly — their reading arose from a mass movement and a conscience-based authority rather than scholarly office, and the orthodox establishment did not admit it to authoritative interpretation. They built parallel institutions (ashrams, movements, presses) rather than leaving the tradition.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_allegorical_adherents, excluded,
    organized, generational, mobile, national).

% Study the discourse and its readings across traditions without adherence to any seat. They trace how the text's authority is constructed, which readings dominate when, and what each reading's adoption would change. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, comparative_hermeneutic_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpretive_class).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the crisis of duty-paralysis: it tells each person what their role requires, coordinates warriors into battle without case-by-case moral deliberation, stabilizes a role-stratified social order under a shared sacred warrant, and supplies a meaning-structure for action under moral uncertainty.
% TRANSFER_FUNCTION: Moves moral agency from individuals to the divine-command structure (conscience is overridden by duty); moves interpretive authority and social precedence to the Brahminical class; moves the human cost of political violence onto conscripted warriors and the war's dead; secures hereditary service obligations from the laboring castes.
% ABSENT_VOICES: The Gandhian allegorical dissenters and the caste-oppressed themselves: under the orthodox arrangement, interpretation is the Brahminical class's monopoly, so those who would read the discourse as inner struggle and those who bear caste's costs have no seat in the conversation; the war's dead also cannot testify. Their exclusion is what the interpretive enforcement exists to maintain.
% DISAPPEARANCE_RATIONALE: If the discourse's orthodox deployment vanished overnight, the legitimation structure for dharmic war and caste duty would collapse: wars would need secular justification, caste hierarchy would lose its divine warrant, and individual conscience would face no command to override it. The text itself would remain, but its operative authority — what it licenses and commands — would stand vacant until a reading filled it, and the beneficiary seats would lose the deference and sanctified rule that flow through their administration of meaning.
% FOUNDING_PROBLEM: The crisis of moral paralysis in duty-bound action: a warrior's refusal to fight his kin threatens the political order, and a role-stratified society needs a warrant that holds. The discourse resolves the crisis by grounding action in divine command and role duty, so that warriors act without individual moral deliberation and the social order holds under a shared sacred warrant.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: the Gandhian allegorical reading itself — a mass-movement, conscience-based seat outside the Brahminical interpretive monopoly — attests the founding problem as the misresolution of inner struggle into war mandate; Ambedkarite Dalit scholarship attests the caste-legitimating function from the victim seat; academic hermeneutic scholarship corroborates the discourse's war-legitimating operation without adherence to either seat. The Brahminical class's own attestation (that the problem is live dharma) is the benefiting parties' self-attestation and is discounted accordingly.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.80 because, on this reading's assessment, the arrangement's core operation is the conversion of persons into role-obligations: conscience is overridden by command, birth is converted into duty, and killing kin is sanctified. Suppression (0.78) is a raw structural property, unscaled by power or scope: it is the interpretive monopoly plus the sanction structure (disgrace, caste exclusion, divine displeasure) that closes alternatives. Alternatives did exist — renunciation, heterodox movements, conversion — but at catastrophic cost, hence accessibility_collapse at 0.55 rather than higher. Theater (0.36) reflects a mostly functional arrangement whose performative share has grown as literal belief eroded and the war-frame became political rhetoric. Resistance (0.60) records two millennia of contest: Buddhist and Jain repudiation, Bhakti bypasses of Brahminical authority, modern reform, the Gandhian reading itself, and Ambedkarite exit — including coalition power among the powerless seats (Dalit mobilization and mass conversion), which is why resistance sits far above the near-zero of a natural law. The measurement series share one grid (T = 0, 20, 43, 60, 84, 100, 120 of an approximately 1905-2025 contest interval). Base extractiveness is U-shaped: the Gandhian contest's rise (non-cooperation era through independence, constitutional abolition of untouchability) depresses the arrangement's effective hold mid-interval; post-institutional decay of Gandhian carriers and the literalist revival (Ayodhya-era dharma-yuddha rhetoric, communal mobilization) drive the rise to 0.80. Theater rises monotonically on the same grid as the war-frame's operational function atrophies into political invocation. suppression_requirement is authored because the story specifically tracks enforcement-capacity change: the interpretive monopoly erodes through the reform era (0.70 to 0.60), then re-hardens with organized literalism (0.78) — enforcement machinery loosened and rebuilt, not merely extraction shifting.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from the same structure. From the Brahminical seat the arrangement is the tradition it administers: meaning, order, and office fused — its identity_locked exit means its computed classification cannot change without an identity break, and the suppression series' post-independence re-hardening is partly the enforcement cost of that fusion holding while belief erodes. From the conscript and conscience seats the same structure is command overriding refusal. Same-level dynamics: conscripts and conscience objectors hold the same nominal power (moderate) but differ in exit — the conscript's refusal means disgrace plus duty-breach (trapped), the objector's means sanction short of social death (constrained) — so their computed positions differ despite equal standing. The two elite seats differ analogously: the aristocracy's constrained exit (estate and rank loss) versus the interpretive class's identity fusion. The excluded Gandhian seat experiences the arrangement as a misreading enforced by office; it bears suppression without being an extraction target, sitting near symmetric. The engine computes all of this from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive d near the beneficiary end: the interpretive class lowest (it sets the rules and collects the deference), the aristocracy higher because it is genuinely dual-positioned — declared beneficiary with secondary payer exposure, since the same discourse that sanctifies its rule conscripts its members. Victims derive d near the target end, amplified by closed exits: conscripts and outcaste castes are trapped (no exit without social death), placing them near the full-target end; conscience objectors are constrained, slightly inside. Spatial scope is continental: at that scale compliance verification is cheap (the sanction structure is local and total) while exit is closed, which the engine's scope modifier reflects for the trapped targets. No directionality overrides are authored: the beneficiary/victim declarations plus exit options carry the structural relationships, and the aristocracy's dual position is declared via secondary_role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two mislabels. Reading the arrangement as pure extraction (snare) would erase the genuine coordination it performed — role-order, meaning-structure, and duty-resolution that a civilization operated on for two millennia, and which this very reading preserves in transmuted form (the inner struggle is still a discipline the text coordinates). Reading it as rope would launder the harm: fratricide legitimation, caste's structural violence, and conscience-override are not coordination costs but asymmetric harms with named payers. The R5 interview shows the founding problem contested rather than dead — the parties dispute whether the problem the arrangement solved (duty-paralysis, role-order) was ever legitimate as posed — so no mandatrophy or zombie flag fires: the mandate is disputed, not outlived. The arrangement is not a scaffold: it declares no sunset and its justification is the steady state, not a transition. It is not a piton either: gains concentrate demonstrably in the interpretive seat, and the cost of fixing it (dismantling the warrant for a civilization's role order) is prohibitive for the seat that could fix it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the kernel gita_kurukshetra_discourse (this file instantiates gandhian_allegorical_reading). Where exactly do the sibling readings diverge from it, and what would adopting a sibling change structurally?',
    'Comparative structural analysis of the three readings'' victim sets, beneficiary sets, and enforcement structures: the orthodox literal reading restores caste mandate and literal war (victims become conscience objectors and the war''s targets; beneficiaries are Brahminical office and warrior elites, with epsilon authored near the low end as dharma rather than extraction); the universalist devotional reading dissolves caste but relocates authority in surrender to divine will, potentially retaining war legitimation under devotion and subordinating the conscience seat.',
    'Adopting the orthodox sibling collapses this story''s epsilon toward the low end and empties the victim set; adopting the universalist sibling removes caste from the beneficiary structure but changes the suppression profile by replacing conscience-sovereignty with surrender. The disagreement is located in two structural elements: the referent of the text''s war command (literal battlefield versus internal struggle) and the locus of interpretive authority (Brahminical office or divine surrender versus individual conscience).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: which kernel reading this is, what siblings would change, and where the disagreement is located.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the suppression binding the conscience-objector and conscript seats structural (divine-command authority, interpretive monopoly, sanction of disgrace) or internalized (duty fused with selfhood so deeply that conscience self-censors even absent enforcement)?',
    'Post-exit suppression trajectory: examine renunciators, converts (Dalit Buddhists), and secularized descendants of the tradition; if duty-compulsion and deference to war-legitimation persist after the enforcement structure is exited, the suppression is partly internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure and travels with the agent after exit; the trapped seats'' entrapment would shift toward identity_locked and the arrangement''s reach would exceed its enforcement infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural versus internalized suppression mechanism for the overridden-conscience seats.').

omega_variable(
    varna_residual_mandate,
    'Does this reading fully evacuate caste''s mandate from the constraint set, or does the founder''s own qualified defense of varna (as non-hereditary calling rather than birth hierarchy, alongside his uncompromising repudiation of untouchability) leave a residual caste constraint inside the reading?',
    'Textual analysis of the Gandhian corpus distinguishing varna from untouchability, cross-checked against Ambedkarite critique; test whether the reading''s caste evacuation extends to varna itself or only to hereditary caste and outcasting.',
    'If a residual varna mandate persists, this reading partially reproduces the extraction it repudiates, the victim set re-expands to include those bound by sanitized varna roles, and the foreclosure relation to the orthodox literal reading weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(varna_residual_mandate, empirical, 'Whether the reading''s caste evacuation is complete or leaves a sanitized varna residue.').

omega_variable(
    allegorical_overlay_question,
    'Does the internal-struggle metaphor genuinely capture the kernel text''s structure, or is the allegorical frame an overlay that domesticates a text whose dramatic setting is a real battlefield and whose speaker issues a real war command?',
    'Hermeneutic analysis independent of both orthodox and Gandhian commitments: whether the text''s own argumentative moves (appeal to duty, the example of the wise, the doctrine of detached action) support the allegorical construal, only the literal one, or underdetermine the choice.',
    'If the allegorical frame is an overlay, this reading''s repudiation of violence is imposed on rather than drawn from the kernel, weakening its foreclosure of the orthodox reading and relocating the contest from textual meaning to interpretive authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(allegorical_overlay_question, conceptual, 'Whether the allegorical reading is discovered in the text or imposed on it.').

omega_variable(
    coordination_cover_question,
    'Is the standing arrangement''s coordination function genuine (role-order, meaning-structure, and duty-resolution that a civilization operated on for two millennia) or cover for extraction (war legitimation and caste enforcement dressed as dharma)?',
    'Comparative-historical test: did role-stratified societies operating without divine-command discourse (Buddhist polities, Jain merchant communities, secular role systems) achieve comparable stability at measurably lower human cost?',
    'If cover, the arrangement reclassifies toward snare and the victim set carries the full weight of the classification; if genuine, the tangled_rope claim holds and part of the measured cost is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cover_question, empirical, 'Whether the orthodox deployment''s coordination story is genuine function or extraction cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(gita_tr_t0, observed).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(gita_tr_t20, observed).
narrative_ontology:measurement(gita_tr_t43, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 43, 0.25).
narrative_ontology:measurement_basis(gita_tr_t43, observed).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(gita_tr_t60, observed).
narrative_ontology:measurement(gita_tr_t84, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 84, 0.32).
narrative_ontology:measurement_basis(gita_tr_t84, observed).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 100, 0.34).
narrative_ontology:measurement_basis(gita_tr_t100, observed).
narrative_ontology:measurement(gita_tr_t120, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 120, 0.36).
narrative_ontology:measurement_basis(gita_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 0, 0.76).
narrative_ontology:measurement_basis(gita_be_t0, observed).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement_basis(gita_be_t20, observed).
narrative_ontology:measurement(gita_be_t43, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 43, 0.69).
narrative_ontology:measurement_basis(gita_be_t43, observed).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement_basis(gita_be_t60, observed).
narrative_ontology:measurement(gita_be_t84, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 84, 0.77).
narrative_ontology:measurement_basis(gita_be_t84, observed).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 100, 0.79).
narrative_ontology:measurement_basis(gita_be_t100, observed).
narrative_ontology:measurement(gita_be_t120, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 120, 0.8).
narrative_ontology:measurement_basis(gita_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(gita_su_t0, observed).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(gita_su_t20, observed).
narrative_ontology:measurement(gita_su_t43, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 43, 0.6).
narrative_ontology:measurement_basis(gita_su_t43, observed).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(gita_su_t60, observed).
narrative_ontology:measurement(gita_su_t84, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 84, 0.74).
narrative_ontology:measurement_basis(gita_su_t84, observed).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 100, 0.76).
narrative_ontology:measurement_basis(gita_su_t100, observed).
narrative_ontology:measurement(gita_su_t120, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 120, 0.78).
narrative_ontology:measurement_basis(gita_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (gita_kurukshetra_discourse), three readings, three constraints with distinct epsilon, victim sets, and enforcement structures. The orthodox literal reading is the upstream historical arrangement — its deployment is the standing arrangement every other reading measures. The Gandhian allegorical reading contests it and renames its victims; the universalist devotional reading converges with the Gandhian on caste's evacuation while relocating authority in surrender. This story's epsilon (0.80) is reading-indexed over the shared referent (the orthodox deployment as this reading assesses it); the orthodox reading would author low epsilon over the same referent. The disagreement IS the classification data, which is why the readings are separate stories linked here rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
