% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Gita Kurukshetra Discourse — Orthodox Literal Reading
 *   domain: religious/textual/ethical
 *
 * SUMMARY:
 *   The Gita's Kurukshetra discourse, under its orthodox literal reading,
 *   presents caste-based duty (svadharma) as divinely ordained and righteous
 *   violence (dharma yuddha) as cosmically justified when a warrior performs
 *   his caste role. Krishna teaches Arjuna that his kshatriya nature requires
 *   him to fight without attachment to the outcome. This reading legitimates
 *   the caste hierarchy as metaphysically necessary and authorizes the
 *   warrior caste to use violence in defense of their dharmic role. The
 *   Brahminical interpretive tradition (Shankara, Ramanuja, and their
 *   lineages) maintains this reading through centuries of commentary,
 *   securing their own position as the authorized explainers of divine truth.
 *   The constraint operates through textual authority, caste hierarchy, and
 *   interpretive monopoly. Alternative readings (Gandhian allegorical,
 *   universalist devotional) are excluded from the orthodox framework, not
 *   refuted.
 *
 * KEY AGENTS:
 *   - Brahminical priestly class: maintains interpretive monopoly, benefits from securing position as authorized explainers of cosmic duty
 *   - Kshatriya warrior caste: benefits from divine justification of their martial role and caste position
 *   - Lower castes (shudra, untouchable): locked into subordinate roles, bearing costs of hierarchy without voice
 *   - Slain enemies on battlefield: victims whose lives are the cost of righteous war
 *   - Women and non-warrior populations: excluded from discourse, their dharma determined elsewhere
 *   - Gandhian reinterpreters: structurally excluded from orthodox framework, would reframe as allegory
 *   - Vedic exegetical tradition: institutional machinery transmitting and defending the orthodox reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.82).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.91).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Gita Kurukshetra Discourse — Orthodox Literal Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/textual/ethical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, 'e082ddfa-b35f-4759-a203-aeb4513456ed').
narrative_ontology:cs_kernel_codification('e082ddfa-b35f-4759-a203-aeb4513456ed', fixed_text).
narrative_ontology:cs_authority_grounding('e082ddfa-b35f-4759-a203-aeb4513456ed', lineage).
narrative_ontology:cs_interpretation_layer_present('e082ddfa-b35f-4759-a203-aeb4513456ed').
narrative_ontology:cs_reading_relation('e082ddfa-b35f-4759-a203-aeb4513456ed', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('e082ddfa-b35f-4759-a203-aeb4513456ed', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('e082ddfa-b35f-4759-a203-aeb4513456ed', foundational, caste_hierarchy_divinely_ordained).
narrative_ontology:cs_axiom_status(caste_hierarchy_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('e082ddfa-b35f-4759-a203-aeb4513456ed', caste_hierarchy_divinely_ordained, deontological).
narrative_ontology:cs_axiom('e082ddfa-b35f-4759-a203-aeb4513456ed', foundational, righteous_violence_in_caste_duty).
narrative_ontology:cs_axiom_status(righteous_violence_in_caste_duty, holdable).
narrative_ontology:cs_axiom_grounding('e082ddfa-b35f-4759-a203-aeb4513456ed', righteous_violence_in_caste_duty, deontological).
narrative_ontology:cs_reference_frame('e082ddfa-b35f-4759-a203-aeb4513456ed', eternal_cosmic_duty_order).
narrative_ontology:cs_drift_state('e082ddfa-b35f-4759-a203-aeb4513456ed', contemporary_post_colonial_legal_equality, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e082ddfa-b35f-4759-a203-aeb4513456ed', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahminical_priestly_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_caste).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_order).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes_shudra_untouchable).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, slain_enemies_arjuna_battlefield).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, women_non_warriors_excluded_discourse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds interpretive monopoly over the Gita text; declares what the orthodox literal reading means and enforces it through ritual authority, scriptural commentary (Bhashya), and control of Vedic knowledge transmission. Benefits by securing their position as the divinely authorized explainers of cosmic duty. Their reading legitimates caste hierarchy as divinely ordained and places them at the apex of the knowledge hierarchy.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahminical_priestly_class, agenda_setter,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, brahminical_priestly_class, beneficiary).

% Receives divine justification for violence and martial duty (svadharma) from Krishna's teaching in the Gita. The orthodox reading legitimates their warrior function as cosmically necessary and righteous when performed without attachment. They benefit from a metaphysical ordering that places their caste in a fixed, honored role. Their refusal to fight is portrayed as betrayal of cosmic order, not moral choice.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_caste, beneficiary,
    powerful, civilizational, constrained, continental).

% The abstract social order itself is a beneficiary in that the text's orthodox reading vindicates it as divinely ordained and eternal. The reading forecloses mobility and alternative orderings by treating caste as metaphysically necessary.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_order, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_order).

% Locked into subordinate roles by the reading's interpretation of svadharma (one's duty according to caste birth). The text, under this reading, assigns them service roles without option or recourse. They bear the cost of maintaining the hierarchy but have no voice in its justification or modification. Their caste identity becomes their existential trap.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes_shudra_untouchable, payer,
    powerless, civilizational, trapped, continental).

% The adversaries on the Kurukshetra battlefield who are killed in the righteous war (dharma yuddha) justified by the text's orthodox reading. They are portrayed as enemies whose destruction is cosmically righteous, not morally problematic. Their lives are the cost of the warrior's duty. The text offers them no recourse or reframing — only death in service to cosmic order.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, slain_enemies_arjuna_battlefield, payer,
    powerless, immediate, trapped, local).

% Women and non-warrior populations are not addressed in the Gita's orthodox literal reading as it applies to Arjuna. They are outside the framework of discourse entirely — their dharma is determined elsewhere, their voice absent from the textual conversation about righteous duty and violence. The constraint operates through their invisibility; they cannot object because they are not recognized as participants in the philosophical problem.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, women_non_warriors_excluded_discourse, excluded,
    powerless, civilizational, trapped, continental).

% Those who advocate allegorical or non-violent readings of the Gita are structurally excluded from the orthodox literal reading framework. They would reframe Kurukshetra as internal struggle and dharma as spiritual duty rather than caste duty. The orthodox reading's authority structure does not admit their reinterpretation as legitimate; they must operate outside or against the constraint.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_reinterpreters, excluded,
    organized, biographical, constrained, continental).

% The centuries-long interpretive lineage (Bhashya commentators, Vedanta schools) that transmits and elaborates the orthodox literal reading. They are the institutional machinery through which the reading is maintained, transmitted, and defended against challenge. Their scholarly authority and textual mastery are the enforcement mechanism.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, vedic_exegetical_tradition, agenda_setter,
    institutional, civilizational, analytical, continental).

% An external analytical seat that examines the reading as a structured constraint without endorsing or refuting its truth claims. Observes how the text is deployed, who benefits, what alternatives are foreclosed, and how the reading maintains itself through authority structures and interpretive monopoly.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, philosophical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__orthodox_literal_reading, brahminical_priestly_class).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__orthodox_literal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves dharmic crisis by providing a cosmic framework for righteous action: a warrior confused about the morality of killing receives teaching that caste duty is divinely ordained, action without attachment is liberation, and violence performed in accordance with one's dharmic role is not morally transgressive. The text coordinates a response to a genuine existential problem: how to reconcile individual conscience with social role and immediate duty.
% TRANSFER_FUNCTION: Transfers interpretive authority and cosmic legitimacy to the Brahminical priestly class (who alone authoritatively explain the text and its meaning). Transfers cosmic justification and legitimate social role to the kshatriya warrior caste (who alone can authoritatively execute righteous violence and lead society). Transfers permanent subordination to lower castes (shudras, untouchables) who are told their role is divinely ordained and cannot be justly changed. Transfers moral standing as cosmic enemies to those killed in dharma yuddha — they are not victims but obstacles to cosmic order. Transfers invisibility and exclusion to women and non-warrior populations who are not addressed in the discourse.
% ABSENT_VOICES: Women (their dharma is determined elsewhere, they are not participants in the Kurukshetra discourse or in the warrior's crisis). Lower castes (shudra, untouchable) — they would argue that caste is not divinely ordained but socially constructed and unjustly imposed, and that their voices are excluded from the authoritative interpretation of what dharma means. Gandhian reinterpreters — they would argue that Kurukshetra is allegory, that dharma means individual spiritual development not caste duty, and that the orthodox literal reading is a misreading weaponized to defend hierarchy. Universalist devotional traditions — they would argue that the path of devotion (bhakti) transcends caste, making the hierarchy spiritually irrelevant. Philosophical critics (post-colonial, Marxist, Dalit scholars) — they would argue that the constraint is a tool of oppression, not cosmic truth.
% DISAPPEARANCE_RATIONALE: Orthodox proponents: if this reading disappeared, the cosmic justification for social hierarchy vanishes, and the kshatriya caste loses divine mandate for leadership. Lower-caste advocates: if this reading disappeared, the caste hierarchy loses its supreme textual legitimation and becomes contestable as mere human construction, enabling legal and social equality. Post-colonial scholars: if this reading disappeared, one major source of modern fundamentalist defense of hierarchy would be removed, but the caste system persists through legal structures, economic inequality, and social practice — disappearing the Gita would not automatically transform society. Universalist readers: if this reading disappeared, nothing is lost, because the text would simply be reread as teaching universal devotion. The constraint's disappearance is genuinely contested because it IS the anchoring justification for the hierarchy itself — remove this reading, and the hierarchy's claim to cosmic necessity collapses, though material inequality might persist through other mechanisms.
% FOUNDING_PROBLEM: Arjuna stands on the Kurukshetra battlefield facing relatives and friends as enemies, paralyzed by moral confusion: he cannot reconcile his dharmic duty as a warrior with his individual conscience against killing his own people. He experiences relativism and despair — duty and morality seem contradictory, and he cannot act. Krishna must restore his capacity for action by teaching him that his caste role (kshatriya) is his dharma, that performance of duty without attachment to the outcome is righteous, and that violence performed in accordance with cosmic order is not morally transgressive.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox Brahminical tradition (Shankara, Ramanuja, Madhva, and their lineages) attests that Arjuna's dharmic crisis is eternally live — every human faces confusion about duty and action, and the text's teaching applies universally. Gandhian tradition (Gandhi, modern social reformers) attests that the founding problem is being misread — Arjuna's real crisis is spiritual (internal struggle), not military, and reading it literally as war-justification corrupts the text's true teaching. Dalit and post-colonial scholars attest that the 'founding problem' as framed by the orthodox reading is historically contingent on a literal battlefield scenario that is no longer live; the constraint persists through institutional inertia and deployment by modern hierarchies, not because Arjuna's original crisis remains true. Historical-textual scholars attest that the founding problem (Arjuna's paralysis) was likely real in its composition context, but the text has been reused and reinterpreted for different purposes across centuries — the founding problem is no longer the lived problem, if it ever was. NO CORROBORATION from outside the Brahminical beneficiary class for the founding problem's perennial vitality; external witnesses (Dalit leaders, Gandhian reformers, post-colonial scholars) all contest whether the problem remains live or has been replaced/transcended.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, contested).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the reading justifies permanent, divinely ordained extraction: caste determines lifetime role, lower castes have no exit, and their subordination is metaphysically necessary. Suppression is higher still (0.91) because alternatives are foreclosed through three mechanisms: (1) the Brahminical interpretive monopoly declares the orthodox reading as THE truth; (2) exit from caste is portrayed as cosmic betrayal, not legitimate choice; (3) reinterpretations are excluded from authorized frameworks. Accessibility collapse is extreme (0.88) because once the caste hierarchy is accepted as divinely ordained, alternative social orders appear metaphysically impossible — the suppression is not merely social but cosmological. Resistance is substantial (0.71) because lower castes have always contested hierarchy through devotional heterodoxy, anti-caste movements (bhakti traditions, Dalit philosophy), and reinterpretations; colonial and post-colonial critique challenges the reading's legitimacy; and modern universalist readings offer coherent alternatives. Theater is low-to-moderate (0.28) at interval end because the constraint's function (justifying hierarchy and violence) is genuine throughout, though over time ritual performances and textual ceremonies increasingly perform the constraint rather than enforce it — modern contexts often invoke the reading symbolically rather than pragmatically enforcing it. Measurements are one shared time grid: every metric is authored at every time point; the grid spans 2400 years (interval 0–2400 = post-Gita textual history). Rising extractiveness reflects accumulation of commentarial layers that deepen the reading's authority; rising suppression reflects hardening of the interpretive monopoly; theater ratio's rise reflects increasing reliance on ritual and textual performance as social enforcement mechanisms weaken under modern legal and democratic pressure.
 *
 * PERSPECTIVAL GAP:
 *   The Brahminical agenda-setter and kshatriya beneficiary experience the constraint as cosmic coordination: a divine truth resolving genuine philosophical confusion about duty and action. From their seats, the structure is legitimate, necessary, and liberating (kshatriya duty is also kshatriya freedom). The lower-caste payer and excluded seats experience the constraint as enforced extraction: their caste role is imposed, not chosen; their exit is foreclosed; their voices are silenced. From their seats, the structure is oppressive and contestable. The engine computes per-seat classifications from power + exit + beneficiary/victim structure: the Brahminical institutional seat should compute as beneficiary with low d (d ~0.1–0.3, full beneficiary end); the lower-caste powerless seat should compute as full target with high d (d ~0.8–1.0); the kshatriya powerful seat should compute as beneficiary-toward-symmetric (d ~0.3–0.5) because they collect honor and justification but also bear the burden of duty and warfare. This divergence is structural, not erroneous — it is the measurement the asymmetry framework exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: (1) Brahminical priestly class — they collect interpretive authority, social prestige, and the power to define cosmic truth; their exit is arbitrage (they could reinterpret, but choose not to; they profit from the orthodox reading remaining fixed); d near 0.0 for analytical power. (2) Kshatriya warrior caste — they collect cosmic justification, social honor, and legitimate use of violence; their exit is constrained (leaving their caste role is portrayed as spiritual death); d ~0.35–0.45 (moderate beneficiary). (3) Caste hierarchy itself (non-agent, vindicated proposition) — the constraint vindicates it as divinely ordained; non-agent so no directionality. Victims: (1) Lower castes — they bear permanent subordination, are told it is divinely necessary, have no exit (trapped, identity-locked as their caste); d ~0.95 (near full target). (2) Slain enemies — immediate victims of righteous war, no recourse; d ~1.0 (full target). (3) Women and excluded populations — bear the cost of the hierarchy through invisibility, no voice; d ~0.85 (high target, but less exposed than explicit lower castes). The structural asymmetry is extreme: the beneficiaries are either institutional (arbitrage exit, can reinterpret if incentivized) or powerful (constrained exit within their caste role, but honored); the victims are powerless and trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows mandatrophy dynamics: the founding problem (Arjuna's dharmic crisis) was perhaps live and genuine when the Gita was composed, representing a real philosophical need to reconcile individual conscience with social duty. Over centuries, the constraint persists long after the problem has shifted or transformed: modern lower castes do not face Kurukshetra war but face systemic discrimination, land dispossession, and social exclusion. Modern kshatriya do not face existential dharmic crises but face legal equality and democratic citizenship. The constraint's founding justification (resolving dharmic confusion in a literal war) has attenuated, yet the reading persists through institutional inertia (the Brahminical commentarial tradition), ritual performance (the text is recited, taught, canonized), and now symbolic deployment (the reading is invoked to justify modern hierarchies, even when material war is not at issue). Theater ratio's rise (0.10 to 0.28) reflects this shift: as the founding problem becomes less live, more of the constraint's operation is theatrical — ritual rehearsal of the reading's authority, textual ceremony, and symbolic affirmation of caste order — rather than pragmatic enforcement of immediate dharmic duty. Modern challenges (Dalit movements, legal prohibitions on caste discrimination, post-colonial reinterpretations) have eroded the constraint's efficacy at the social level, but it persists in institutional memory and symbolic deployment. This is piton-stage emergence: the constraint has retained shape through inertia and performance, even as the foundational problem has become contested or obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_allegorical_boundary,
    'Is the Gita''s teaching about dharma and violence intended as literal prescription for caste duty and righteous war, or as allegory for spiritual development applicable to all?',
    'Historical-textual analysis of the Gita''s likely original meaning in its Iron Age Vedic context vs. the reading''s deployment in later centuries. Comparison with how other sacred texts (Torah, Bible, Quran) handle war and social hierarchy (literal vs. allegorical disputes). Analysis of whether the orthodox reading''s literalism is consistent with the Gita''s own philosophical complexity or selective literalism that ignores other passages (e.g., teachings on universal brahman, equality in devotion).',
    'If the text is primarily allegorical/metaphorical, the orthodox literal reading is a misreading that weaponizes caste hierarchy by false literalism — the constraint becomes a snare of textual misappropriation, not a legitimate reading. If the text is genuinely ambiguous (as the universalist and Gandhian readings assert), then the orthodox reading is ONE contested interpretation, not THE truth — the constraint becomes contestable rather than cosmologically necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literal_vs_allegorical_boundary, conceptual, 'Dispute about whether the text''s true meaning is literal prescription or metaphorical teaching.').

omega_variable(
    divine_ordination_vs_social_construction,
    'Is the caste hierarchy truly divinely ordained and metaphysically necessary (as the orthodox reading claims), or is it a socially constructed human arrangement that the Gita''s teaching can legitimate but did not create?',
    'If lower castes can be reborn as upper castes (as karma doctrine allows), is caste truly eternally binding? If the text teaches that brahman is universal and all selves are equal in essence, does caste hierarchy stand? Historical evidence: did caste hierarchy predate the Gita, or did the Gita create it? If preexisting, the Gita legitimates but does not ordain; if postdating, did the Gita''s teaching actually produce it? Modern evidence: if caste can be abolished (as Indian law attempts), was it ever truly cosmic necessity?',
    'If caste is divinely ordained, the hierarchy is metaphysically necessary and cannot be justly abolished — the victims are ontologically trapped. If caste is socially constructed and merely legitimated by the text, the hierarchy is contestable and can be dismantled without cosmic violation — the constraint becomes snare (pure extraction), not tangled rope (genuine coordination + asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_ordination_vs_social_construction, conceptual, 'Whether caste hierarchy is divinely necessary or socially contingent.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.91) primarily structural (legal prohibitions on caste mobility, economic barriers, geographical segregation, ritual exclusion maintained by ritual specialists) or internalized (the victims believe their subordination is deserved, divinely ordained, impossible to escape)?',
    'Post-exit suppression trajectory: if lower-caste individuals leave the geographic/institutional context (migrate, convert, engage with outside legal systems, connect with Dalit movements), does the suppression persist? If it does, it is partially internalized (the victim carries the suppression with them). If it does not, the suppression was primarily structural. Historical evidence from Dalit conversion movements, migration patterns, and affirmative-action beneficiaries: do they report persistence of subordination after structural exit, or liberation? Measurement of internalized shame, belief in caste karma, and self-imposed role restriction among lower castes vs. among outsiders to the system.',
    'If primarily structural, the constraint can be dismantled by removing institutional/legal barriers, and victims could exit if barriers fell. If internalized, the suppression is deeper and removal of structural barriers alone is insufficient — the victims would need consciousness-raising, reframing, and identity reconstruction. The constraint''s true suppression magnitude is higher if internalized; it is a snare with cognitive capture, not merely institutional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression persists after structural barriers are removed (indicating internalization).').

omega_variable(
    brahminical_monopoly_enforceability,
    'The Brahminical interpretive monopoly (agenda-setter role) depends on their institutional control of textual transmission and ritual authority. How much of the suppression (0.91) actually requires active Brahminical enforcement, vs. how much is maintained by lower-caste self-policing, state infrastructure, or non-Brahminical elites?',
    'Historical case studies: in periods when Brahminical institutions weakened (certain pre-modern regions, colonial disruption, post-colonial state secularization), did the caste hierarchy weaken? When alternative interpretive communities gained power (bhakti saints, Dalit leaders, legal reformers), did caste subordination decline? Analysis of who actually enforces caste rules in modern contexts — is it still Brahminical priests, or police, employers, community councils with mixed caste membership?',
    'If Brahminical enforcement is essential, the constraint is genuinely agenda-setter-dependent and could be dismantled by removing Brahminical institutional power — it becomes a Tangled Rope with a clear enforcer to target. If the constraint is maintained primarily by non-Brahminical elites and lower-caste internalization, then the Brahminical role is performative or historically residual; the constraint is more diffusely enforced (piton-like, maintained by inertia rather than active coercion). This affects where power to change the constraint actually lies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_monopoly_enforceability, empirical, 'Whether Brahminical institutional enforcement is necessary to maintain the caste suppression, or whether other mechanisms suffice.').

omega_variable(
    kernel_reading_ambiguity,
    'Does the Gita kernel admit the orthodox literal reading as a legitimate exegesis, or does the kernel''s complexity foreclose literal caste-based reading as a genuine interpretation?',
    'Textual analysis: the Gita explicitly teaches universal brahman, equality of selves in essence, and paths of devotion open to all. Does the orthodox reading''s literalism about caste duty square with these teachings, or does it require selective literalism (caste is literal, but brahmanic universalism is metaphorical)? If selective, is the reading internally coherent or does it contain contradictions that mark it as a motivated misreading rather than genuine exegesis? What do the earliest Gita commentators (Bhagavata Purana, early Bhashyas) say — do they also read caste literally, or do they qualify it?',
    'If the reading is a motivated misreading (internally contradictory, selective literalism that violates the text''s own logic), then the constraint is maintained by institutional power and textual authority claims, not by genuine exegetical truth. The Brahminical monopoly becomes more obviously a snare (extractive power dressed in textual legitimacy) rather than a legitimate reading. If the reading is internally coherent and genuinely supported by the text, then at least the kernel admits it as a live interpretation — the constraint is contestable but not false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the orthodox literal reading is a genuine exegesis or a motivated misreading of the Gita.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 2400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gita_tr_t400, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 400, 0.14).
narrative_ontology:measurement(gita_tr_t800, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 800, 0.18).
narrative_ontology:measurement(gita_tr_t1200, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement(gita_tr_t1600, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1600, 0.25).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(gita_tr_t2400, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2400, 0.28).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(gita_be_t400, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 400, 0.71).
narrative_ontology:measurement(gita_be_t800, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 800, 0.76).
narrative_ontology:measurement(gita_be_t1200, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1200, 0.79).
narrative_ontology:measurement(gita_be_t1600, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1600, 0.81).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(gita_be_t2400, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2400, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(gita_su_t400, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 400, 0.81).
narrative_ontology:measurement(gita_su_t800, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 800, 0.85).
narrative_ontology:measurement(gita_su_t1200, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1200, 0.88).
narrative_ontology:measurement(gita_su_t1600, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1600, 0.9).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2000, 0.91).
narrative_ontology:measurement(gita_su_t2400, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2400, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.12).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__universalist_devotional_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, brahminical_priestly_authority_system).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_vedic_justification).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel gita_kurukshetra_discourse. Sibling constraints: gandhian_allegorical_reading (Kurukshetra as internal spiritual struggle, violence as metaphor, universality of dharma) and universalist_devotional_reading (bhakti path open to all, caste irrelevant to salvation). These three readings are not three measurements of one constraint; they are three structurally distinct constraints grounded in different interpretations of the same text. The orthodox_literal_reading has high extractiveness (0.82) because it justifies permanent caste hierarchy; the Gandhian reading has lower extractiveness (~0.4–0.5) because it universalizes dharma and makes violence metaphorical; the universalist reading has minimal extractiveness (~0.2–0.3) because it focuses on devotional path open to all. Different ε values mark different constraints, not different measurements of one constraint (DP-001 ε-invariance principle). The three constraints are linked through network.affects_constraints because the orthodox reading influences (constrains, delegitimizes) the sibling readings by claiming interpretive monopoly on the text's true meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
