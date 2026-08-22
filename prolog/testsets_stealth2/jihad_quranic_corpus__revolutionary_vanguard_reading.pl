% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Immediate Individual Obligation Against Apostate Rulers and Occupiers (Revolutionary Vanguard Reading)
 *   domain: religious/political-theological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the jihad_quranic_corpus kernel:
 *   the revolutionary vanguard reading, in which defensive war against
 *   apostate rulers and occupying forces is each believer's immediate
 *   personal obligation, valid without state or imam authorization, with the
 *   victim set expanded by takfir (declaring fellow Muslims unbelievers) and
 *   by emergency jurisprudence that suspends the classical safeguards of
 *   invitation-first, proportionality, and non-combatant immunity. Per the
 *   epsilon-referent rule, the epsilon authored here measures THIS
 *   arrangement — the decentralized obligation structure as it actually
 *   operates through the movements that hold it — and not the sibling
 *   readings' arrangements (the defensive-spiritual regime of restrained,
 *   state-conditioned response, or the classical legalist regime of
 *   imam-authorized campaigns), which are separate constraint stories linked
 *   through the network section. The colloquial label 'jihad' decomposes into
 *   these structurally distinct claims with materially different victim sets,
 *   authority loci, and safeguard profiles; forcing them into one story would
 *   make epsilon observable-dependent, violating epsilon-invariance.
 *
 * KEY AGENTS:
 *   - vanguard_ideologues: Agenda-setting authority (organized/identity_locked) — authors the obligation, issues takfir rulings, collects interpretive control while bearing minimal operational risk
 *   - insurgent_cell_leadership: Concentrated beneficiary (organized/identity_locked) — converts the license into networks, rank, and resourced followings
 *   - insurgent_foot_soldiers: Primary extracted seat (powerless/trapped) — supplies the lives the arrangement spends; nominally honored, structurally consumed
 *   - civilian_populations_in_conflict_zones: Broadest victim seat (powerless/trapped) — redesignated as enemy by collective-guilt attribution, no voice in the designation
 *   - declared_apostate_rulers: Institutional target seat (institutional/constrained) — delegitimized and hunted; their repression feeds the recruitment cycle
 *   - occupying_forces: Mobile target seat (institutional/mobile) — designated standing targets; exit by withdrawal exists but is politically costly
 *   - mainstream_ulama: Excluded counter-authority (institutional/identity_locked) — holds the safeguard tradition, locked out of the movement's interpretive space, assassinated when audible
 *   - counterterrorism_analysts: Analytical observer (analytical/analytical) — sees the full structure from outside its commitments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.8).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Immediate Individual Obligation Against Apostate Rulers and Occupiers (Revolutionary Vanguard Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious/political-theological").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, 'ae6e09fa-a250-4253-adc2-3f8ad80383cd').
narrative_ontology:cs_kernel_codification('ae6e09fa-a250-4253-adc2-3f8ad80383cd', fixed_text).
narrative_ontology:cs_authority_grounding('ae6e09fa-a250-4253-adc2-3f8ad80383cd', extraction).
narrative_ontology:cs_interpretation_layer_present('ae6e09fa-a250-4253-adc2-3f8ad80383cd').
narrative_ontology:cs_reading_relation('ae6e09fa-a250-4253-adc2-3f8ad80383cd', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('ae6e09fa-a250-4253-adc2-3f8ad80383cd', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_axiom('ae6e09fa-a250-4253-adc2-3f8ad80383cd', foundational, individual_obligation_without_state_authorization).
narrative_ontology:cs_axiom_status(individual_obligation_without_state_authorization, holdable).
narrative_ontology:cs_axiom_grounding('ae6e09fa-a250-4253-adc2-3f8ad80383cd', individual_obligation_without_state_authorization, deontological).
narrative_ontology:cs_axiom('ae6e09fa-a250-4253-adc2-3f8ad80383cd', foundational, takfir_dissolves_muslim_immunity).
narrative_ontology:cs_axiom_status(takfir_dissolves_muslim_immunity, holdable).
narrative_ontology:cs_axiom_grounding('ae6e09fa-a250-4253-adc2-3f8ad80383cd', takfir_dissolves_muslim_immunity, empirically_contingent).
narrative_ontology:cs_axiom('ae6e09fa-a250-4253-adc2-3f8ad80383cd', secondary, emergency_suspends_proportionality_safeguards).
narrative_ontology:cs_axiom_status(emergency_suspends_proportionality_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('ae6e09fa-a250-4253-adc2-3f8ad80383cd', emergency_suspends_proportionality_safeguards, instrumental).
narrative_ontology:cs_reference_frame('ae6e09fa-a250-4253-adc2-3f8ad80383cd', permanent_defensive_emergency).
narrative_ontology:cs_drift_state('ae6e09fa-a250-4253-adc2-3f8ad80383cd', post_isis_territorial_defeat, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ae6e09fa-a250-4253-adc2-3f8ad80383cd', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_ideologues).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, insurgent_cell_leadership).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, declared_apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, insurgent_foot_soldiers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_ulama).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, insurgent_foot_soldiers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Self-taught preachers and strategists in the lineage of Sayyid Qutb, Abd al-Salam Faraj, and Abu Muhammad al-Maqdisi. They publish rulings declaring sitting rulers unbelievers, rule that defensive war is each believer's personal and immediate duty, and license autonomous cells to act without any state or institutional authorization. Authority flows to them from believers who accept these rulings; they bear little personal operational risk, living dispersed or in exile while others fight. Abandoning the position would dissolve the sole source of their standing.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_ideologues, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_ideologues, beneficiary).

% Mid-level commanders who run training, finances, and recruitment for autonomous cells. They convert the doctrine's license into operational networks, gaining status, control of donated resources, and followings of their own. Mortality falls overwhelmingly on those they deploy rather than on themselves; stepping back would cost them rank, resources, and purpose within the movement.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, insurgent_cell_leadership, beneficiary,
    organized, biographical, identity_locked, global).

% Young men recruited through study circles, prisons, and online networks. They carry out the attacks the doctrine requires and are expected to die doing it; the movement celebrates their deaths as martyrdom and pays stipends to their families. Family ties, surveillance by both security services and their own comrades, and the ruling that withdrawal equals unbelief close the way back.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, insurgent_foot_soldiers, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, insurgent_foot_soldiers, beneficiary).

% Residents of the cities and regions where cells operate — markets, funerals, mosques, commuter trains. The doctrine counts them among the enemy when they live under governments it has condemned or alongside forces it fights, so they absorb the bombings on both sides of the resulting wars. They have no seat anywhere in the argument that redesignates them, and flight means destitution in camps or hostile borders.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_in_conflict_zones, payer,
    powerless, generational, trapped, regional).

% Presidents, kings, and senior officers of Muslim-majority states whom the doctrine brands unbelievers for governing by man-made law and allying with Western powers. Assassination attempts, insurgencies, and terror campaigns pursue them and their officials. Conceding to the doctrine's demands would validate its logic and invite the next escalation, so they respond with repression that in turn feeds recruitment.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, declared_apostate_rulers, payer,
    institutional, biographical, constrained, national).

% Foreign militaries stationed in Muslim-majority lands, from Cold War interventions to the post-2001 deployments. The doctrine names their presence a standing cause of war and their personnel legitimate targets wherever found. They can withdraw — and withdrawal is precisely what the doctrine demands — but withdrawing under attack carries severe political costs at home, so they oscillate between escalation and drawdown while their casualties mount.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces, payer,
    institutional, biographical, mobile, continental).

% State-linked and independent scholars at al-Azhar, the fiqh academies, and the madrasas who hold that war requires legitimate authorization, that non-combatants are immune, and that declaring specific living people unbelievers is forbidden. Their refutations are answered with bullets — hundreds of imams and scholars who publicly opposed the doctrine have been assassinated — and the movement's media channels strip them of standing among radicalized youth. Their scholarly identity binds them to keep refuting regardless of the cost.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_ulama, excluded,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_ulama, payer).

% Academic researchers and security analysts who trace the doctrine's texts, financing, and recruitment pipelines across jurisdictions. They see the whole structure — the rulings, the cells, the casualty ledger — from outside any of its commitments, and their assessments feed both scholarship and policy.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, counterterrorism_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_ideologues).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispersed believers into armed action without state command structures: recruitment through study circles and online media, indoctrination curricula, takfir rulings that designate enemies, and cell autonomy that lets operations survive the capture or killing of any leader. For a transnational community that formal religious and state institutions refuse to authorize for war, the arrangement solves a real mobilization problem.
% TRANSFER_FUNCTION: Moves lives, money, and legitimacy. It transfers mortal risk from ideologues and commanders onto foot soldiers and civilian bystanders; moves donations and recruits from sympathizer communities into operational cells; moves interpretive authority from credentialed scholarly institutions to self-appointed preachers; and moves security, stability, and communal trust out of the societies where it operates.
% ABSENT_VOICES: The mainstream scholarly establishment and the civilian populations redesignated as enemy are absent. Victims of takfir rulings cannot contest their own designation inside the framework, because the doctrine rules that objecting to a correct ruling is itself evidence of unbelief — the appeal procedure terminates at the accuser.
% DISAPPEARANCE_RATIONALE: If the fard 'ayn authorization and its takfir machinery vanished overnight, thousands of active cells lose the narrative that licenses their operations, recruitment pipelines built on the neglected-duty argument collapse, assassination campaigns against rulers and attacks on occupier forces shrink to ordinary criminal violence, and the theological barrier that forbids killing fellow Muslims snaps back into place across the movement's base.
% FOUNDING_PROBLEM: The perceived catastrophe of the ummah: colonial occupation, the abolition of the caliphate in 1924, and the consolidation of secular or Western-aligned states left pious believers, as they saw it, under unbeliever or apostate domination with no lawful means of resistance — because classical doctrine conditioned war on an imam's authorization that no legitimate imam existed to give.
% FOUNDING_PROBLEM_CORROBORATION: The underlying grievances have real corroboration from outside the movement: the historical record documents the occupations and the 1924 abolition, and human-rights organizations document authoritarian governance and foreign military presence in Muslim-majority lands today. But no source outside the benefiting parties attests that these conditions license individualized killing without authorization — al-Azhar, the International Islamic Fiqh Academy, and the overwhelming weight of juristic opinion explicitly deny that inference, and the movements' own defection literature increasingly disputes it from inside.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.88) because the arrangement converts believers' lives and civilians' security into movement capability at catastrophic rates: the casualty ledger runs across five distinct seats, and the classical safeguards that once capped the costs (authorization, invitation, immunity, proportionality) are precisely what the emergency doctrine strips out. Suppression is high (0.80) because persistence depends on active enforcement — takfir against internal dissent, execution of deserters, assassination of opposing clergy, and denunciation of rival readings as defeatism — not on voluntary preference. Theater ratio is moderate and rising (0.45): martyrdom media, online spectacle, and utopian branding are load-bearing for recruitment, but the underlying violence is functionally real, so performance supplements rather than replaces operation. Accessibility collapse is partial (0.62): inside the reading's own premises the alternatives collapse almost completely (the spiritual reading becomes dereliction, the legalist reading becomes cowardice), yet rival readings retain enormous institutional support across most of the Muslim world, so the collapse is frame-relative rather than global. Resistance is high (0.78): states, scholarly establishments, and affected populations actively fight the arrangement everywhere it operates. Claim and metrics were authored independently: the snare claim rests on structure — a defense-of-the-ummah coordination story whose actual operation concentrates authority gains on a small ideologue-and-commander class while diffusing catastrophic costs across civilians, conscripts, and condemned societies — while the metrics describe observed operation without reference to the claim.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from the same texts. From the ideologue seat the arrangement is liberation: a duty restored, chains of illegitimate authority broken, the believer returned to direct covenant. From the foot-soldier seat it is a totalizing claim on his body with a one-way door. From the civilian seat it is a death sentence issued by strangers in a language of piety. From the ruler's seat it is a delegitimation machine that makes every concession fatal. From the mainstream scholar's seat it is a forged warrant — classical jurisprudence read selectively to erase fourteen centuries of safeguard doctrine. The engine computes these divergences from the structural data (power, exit, role); nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Vanguard ideologues sit nearest the beneficiary pole: authority and donated resources flow in, operational risk flows out, and their identity-lock means the arrangement subsidizes them for as long as they hold it. Cell leadership shares the subsidy at one remove. Foot soldiers are listed among those borne down despite the martyrdom reward because the net flow is their lives out; the secondary beneficiary role registers the meaning and status they receive, but the derivation correctly weights the consumed life over the conferred meaning. Civilians sit at the full-target pole: pure cost, no exit, no voice. Apostate rulers are heavy targets with constrained exit — they cannot concede without validating the logic hunting them. Occupying forces are targets whose mobility moderates effective extraction somewhat: withdrawal exists as an exit the doctrine itself demands. Mainstream ulama combine exclusion with payment — locked out of the conversation and killed for speaking — placing them near the target pole despite institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement's warrant is the doctrine of necessity (darura): an emergency provision, classically understood as strictly bounded — necessity expires when the necessity ends. Over the measured interval the emergency became permanent infrastructure: the exception stopped being an exception and became the operating system, which is the mandatrophy signature in its purest form. The founding problem (occupation, unrepresentative governance) retains enough lived reality that its status is contested rather than dead, so the dead-mandate-plus-world-rearranges mismatch flag does not fire cleanly; instead the pathology shows up as the emergency that never sunsets and the victim set that only ever expands. Classifying this as a snare rather than dismissing it as mere criminality keeps the genuine grievance core visible (which matters for counter-recruitment), while refusing the coordination cover for what the safeguard-stripping actually does: it is the stripping itself that constitutes the transfer, since every stripped safeguard relocates cost from the movement onto people with no seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the jihad_quranic_corpus kernel: does the vanguard reading''s selective-citation method (sword verses read as abrogating and unconditional) or the siblings'' holistic-exegesis method better account for the corpus as a whole?',
    'Comparative tafsir and usul al-fiqh analysis tracing how each reading handles the same verse set (9:5, 9:29, 2:190, 4:90, the treaty and immunity passages) and which method the classical tradition''s own meta-rules endorse.',
    'If the siblings'' method is textually stronger, this arrangement loses its legitimating frame and its authority structure collapses into bare coercion; if the vanguard method holds, the sibling readings'' narrower victim sets and authorization requirements are the constructs requiring explanation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the vanguard reading''s exegetical method or its siblings'' better accounts for the shared kernel.').

omega_variable(
    takfir_boundary_expansion,
    'Is the apostate-designation boundary structurally stable, or is takfir an endlessly expandable mechanism whose subject class necessarily grows (rulers, then their soldiers, then taxpayers, then scholars who object, then rival jihadists)?',
    'Longitudinal coding of takfir rulings across the movement''s history: track the expanding sequence of designated classes and test whether any internal doctrinal resource reliably bounds the expansion.',
    'If expansion is structural, the victim set is unbounded and the arrangement''s extractiveness is understated at any fixed measurement; if a stable bound exists, part of the measured cost is doctrinally capped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(takfir_boundary_expansion, empirical, 'Whether the takfir mechanism has a stable boundary or expands without limit.').

omega_variable(
    emergency_authenticity,
    'Is the perpetual emergency that justifies suspending the classical safeguards responsive to actual existential conditions, or is it self-manufactured — the arrangement''s own violence generating the repression and chaos that then certify the emergency?',
    'Comparative analysis across periods and theaters: test whether safeguard-relaxation tracks independently-measured threat intensity or tracks the movement''s own operational tempo, and examine whether the doctrine has ever relaxed safeguards when conditions improved.',
    'If the emergency is manufactured, the emergency jurisprudence is the extraction mechanism itself wearing a warrant, and the safeguard-stripping should be scored as the core transfer rather than a side effect; if genuine, part of the suspended-safeguard cost is attributable to real conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_authenticity, empirical, 'Whether the perpetual darura is externally warranted or self-sustaining.').

omega_variable(
    collective_guilt_reach,
    'How far does the collective-guilt attribution extend — which civilian classes become targetable under the emergency doctrine, and does the class definition vary by theater, sect, and strategic convenience?',
    'Content analysis of casualty-justification texts across theaters (Maqdisi''s treatises, theater-level justifications of sectarian and market bombings) cross-referenced with actual target selection.',
    'Sets the true breadth of the victim set: a sect- or convenience-varying boundary indicates the guilt doctrine is a flexible instrument of expansion rather than a fixed rule, raising effective extractiveness above any static estimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_guilt_reach, empirical, 'The reach and stability of the collective-guilt mechanism that redesignates civilians.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the foot-soldier seat''s inability to exit is structural (surveillance, deserter execution, family exposure) versus internalized (martyrdom socialization, identity fusion, sunk-cost commitment)?',
    'Post-exit trajectories of defectors: if suppression symptoms persist after physical escape and amnesty, the internalized share is substantial; rapid reintegration indicates the structural share dominates.',
    'If internalized, the arrangement''s effective hold exceeds the structural measure — exit exists on paper but not in the person — and counter-recruitment must address identity rather than only physical safety.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized components of retention among foot soldiers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_vanguard_reading_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t60, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(jihad_vanguard_reading_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jihad_vanguard_reading_be_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(jihad_vanguard_reading_be_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(jihad_vanguard_reading_be_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(jihad_vanguard_reading_be_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(jihad_vanguard_reading_be_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 50, 0.86).
narrative_ontology:measurement(jihad_vanguard_reading_be_t60, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 60, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jihad_vanguard_reading_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(jihad_vanguard_reading_su_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(jihad_vanguard_reading_su_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(jihad_vanguard_reading_su_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(jihad_vanguard_reading_su_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(jihad_vanguard_reading_su_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(jihad_vanguard_reading_su_t60, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 60, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'jihad' decomposes into three structurally distinct arrangements instantiating one kernel (jihad_quranic_corpus). The expansionist_legalist_reading is the historical upstream: classical fiqh's conditioned campaign regime, whose authorization and immunity architecture the vanguard reading cites and then negates. The defensive_spiritual_reading is the majority-practice baseline from which the vanguard reading recruits, reframing restraint as dereliction. Epsilon differs sharply across the family: negligible-to-moderate for the defensive reading (restraint architecture), moderate for the legalist reading (real extraction potential bounded by safeguards), maximal for this reading (safeguards stripped, victim set expanded by takfir and collective guilt). Each member is a separate story with its own epsilon, beneficiaries, victims, and classification; this file links to both siblings via affects_constraints, and the foreclosure relations are recorded in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
