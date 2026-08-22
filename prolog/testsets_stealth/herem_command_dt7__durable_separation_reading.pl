% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem as Timeless Covenant Separation Mandate (Durable Separation Reading)
 *   domain: religious/hermeneutic/commitment-system
 *
 * SUMMARY:
 *   Under the durable separation reading, Deuteronomy 7's herem command
 *   operates as a standing normative regime: bounded membership, categorical
 *   separation from designated outsiders, prohibition of intermarriage, and a
 *   divine-command register that legitimizes coercive enforcement. This file
 *   instantiates ONE reading of the contested kernel herem_command_dt7 as a
 *   clean, epsilon-invariant constraint; the contextual supersession and
 *   allegorical displacement readings are separate constraints in separate
 *   files, linked through network edges. The epsilon referent is the standing
 *   arrangement under contest — the lived separation regime in communities
 *   that hold this reading — assessed as this reading's own framework
 *   encounters it: the reading asserts the mandate is timeless and good,
 *   while the structural data record who pays and what is suppressed. The
 *   claim/metrics split is deliberate: claimed_type is asserted from
 *   structural analysis; the metrics describe observed operation
 *   independently. KEY AGENTS (by structural relationship): -
 *   religious_authority_class: Agenda-setter and primary beneficiary
 *   (institutional/constrained) — administers the boundary, collects
 *   deference and adjudication power - covenant_community_members:
 *   Dual-positioned beneficiary/payer (moderate/identity_locked) — receives
 *   continuity, pays autonomy - marriage_age_community_members: Primary
 *   target (powerless/trapped) — bears the rule where it bites hardest -
 *   non_covenant_outsiders: Designated victims, excluded from the
 *   conversation (organized/mobile) — categorized without consent -
 *   exogamous_couples_and_offspring: Secondary targets (powerless/trapped) —
 *   bear sanction and inherited stigma - hermeneutics_scholars: Analytical
 *   observer (analytical/analytical) — sees the full structure, collects
 *   nothing
 *
 * KEY AGENTS:
 *   - religious_authority_class: agenda-setting administrator and concentrated beneficiary (institutional power, constrained exit)
 *   - covenant_community_members: dual beneficiary/payer with identity-locked exit (moderate power)
 *   - marriage_age_community_members: primary payer at the rule's sharpest point (powerless, trapped)
 *   - non_covenant_outsiders: designated victims excluded from the conversation (organized, mobile)
 *   - exogamous_couples_and_offspring: sanctioned mixed unions and stigmatized offspring (powerless, trapped)
 *   - hermeneutics_scholars: analytical observer seat (civilizational horizon)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.82).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.8).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem as Timeless Covenant Separation Mandate (Durable Separation Reading)").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious/hermeneutic/commitment-system").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '3a78438e-4de4-4172-9afd-74a280b64f4b').
narrative_ontology:cs_kernel_codification('3a78438e-4de4-4172-9afd-74a280b64f4b', fixed_text).
narrative_ontology:cs_authority_grounding('3a78438e-4de4-4172-9afd-74a280b64f4b', lineage).
narrative_ontology:cs_interpretation_layer_present('3a78438e-4de4-4172-9afd-74a280b64f4b').
narrative_ontology:cs_reading_relation('3a78438e-4de4-4172-9afd-74a280b64f4b', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('3a78438e-4de4-4172-9afd-74a280b64f4b', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('3a78438e-4de4-4172-9afd-74a280b64f4b', foundational, herem_binding_in_every_generation).
narrative_ontology:cs_axiom_status(herem_binding_in_every_generation, holdable).
narrative_ontology:cs_axiom_grounding('3a78438e-4de4-4172-9afd-74a280b64f4b', herem_binding_in_every_generation, theological).
narrative_ontology:cs_axiom('3a78438e-4de4-4172-9afd-74a280b64f4b', secondary, intermarriage_dissolves_covenant_identity).
narrative_ontology:cs_axiom_status(intermarriage_dissolves_covenant_identity, holdable).
narrative_ontology:cs_axiom_grounding('3a78438e-4de4-4172-9afd-74a280b64f4b', intermarriage_dissolves_covenant_identity, theological).
narrative_ontology:cs_reference_frame('3a78438e-4de4-4172-9afd-74a280b64f4b', sinaitic_timeless_boundary_mandate).
narrative_ontology:cs_drift_state('3a78438e-4de4-4172-9afd-74a280b64f4b', contemporary_pluralist_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3a78438e-4de4-4172-9afd-74a280b64f4b', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, religious_authority_class).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, marriage_age_community_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, non_covenant_outsiders).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, exogamous_couples_and_offspring).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, covenant_community_members).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, divine_command_obedience_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, covenant_election_separation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates who may marry whom, reviews cases of forbidden contact, authorizes or annuls membership, and teaches the separation rules as binding in every generation. Collects deference, decision-making power over the community's most intimate choices, and the central place in communal life that the boundary system creates. Their status and vocation exist only inside the arrangement they administer; leaving it would cost them everything their formation built.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, religious_authority_class, agenda_setter,
    institutional, generational, constrained, global).

% Receive continuity of identity, mutual aid networks, schooling, and a total framework of meaning from living inside the boundary. Pay in marriage choice, cross-boundary friendship, career geography, and submission to communal discipline. Raised from birth inside the rules, they experience the boundary as the shape of the world itself; leaving would mean losing family, community, and the interpretive frame that gives their life coherence.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_members, beneficiary,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, covenant_community_members, payer).

% Meet the rule at its sharpest point: attachment to someone outside the boundary triggers family crisis, communal sanction, and a choice between the person and everything else. Economically and socially dependent on parents and community, most comply through chaperoned matchmaking; some leave and absorb the full identity cost; a few maintain hidden relationships under monitoring of communications and associations.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, marriage_age_community_members, payer,
    powerless, biographical, trapped, regional).

% Are designated as contamination threats by texts and teachings they did not author and are rarely consulted about. Bear stigma, avoidance rules governing commerce and neighborliness, and — in the reading's historical register — legitimation of dispossession and violence done to similarly designated populations. Most never learn the detailed rules that classify them; those who encounter the boundary as neighbors, suitors, or in-laws hit it without appeal.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, non_covenant_outsiders, excluded,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, non_covenant_outsiders, payer).

% Mixed couples face shunning, formal mourning observances in some communities, exclusion from communal schools and worship, and pressure on the outside spouse to accept conversion on terms the community sets. Offspring inherit ambiguous status under descent and conversion rules and grow up managing a split inheritance of belonging and stigma.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, exogamous_couples_and_offspring, payer,
    powerless, biographical, trapped, regional).

% Study the text's composition context, reception history, and comparative ancient Near Eastern parallels; track which communities enforce which readings and with what consequences. Hold no seat in the enforcement structure and collect nothing from its operation; their analyses circulate mostly outside the communities that live under the reading.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, hermeneutics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, religious_authority_class).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains bounded membership and intergenerational continuity for a minority covenant community: standardized marriage rules, membership criteria, and separation practices solve the collective-action problem of assimilation drift that individual families cannot solve alone.
% TRANSFER_FUNCTION: Moves marriage choice and associational freedom from community members to the boundary-maintenance apparatus; moves deference and adjudication power to the religious authority class; in the historical register, moved land, property, and life from designated outsider populations toward the covenant community; moves stigma onto exogamous unions and their offspring.
% ABSENT_VOICES: Designated outsiders are categorized without representation — the constraint defines them as contamination threats in their absence. Women and marriage-age youth in traditional settings had no seat where boundary rules were authored. Internal dissenters who read the text as historically bounded or figurative are marginalized as unfaithful rather than engaged.
% DISAPPEARANCE_RATIONALE: Communities holding this reading organize residence, schooling, courtship, friendship, and worship around the boundary; overnight removal would immediately rearrange marriage patterns, communal institutions, and the authority structure's reason for being — while communities not holding the reading would notice nothing, which is itself diagnostic of how much of the arrangement is local construction rather than natural fact.
% FOUNDING_PROBLEM: Securing a small settlement-era community's survival and distinct covenant identity amid larger, religiously absorptive neighboring societies — preventing dissolution through intermarriage and cultic assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical scholarship outside the benefiting parties corroborates the settlement-era genesis and the reality of the ancient assimilation threat; sociology of religion corroborates that assimilation pressure on diaspora minorities persists. No source outside the tradition's own benefiting parties attests the further claim that the mandate is timeless rather than historically bounded — that element rests on the tradition's self-attestation alone.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.82 at interval end) because the regime strips marriage and associational autonomy from insiders while imposing categorical designation on outsiders who never consented to the classification; the violence-legitimation register adds a latent coercive layer no ordinary boundary norm carries. Suppression (0.80) reflects enforcement through sanction machinery, economic dependence, educational closure, and identity fusion rather than participant preference. Theater is moderate-low (0.28) in the current revival phase: most enforcement activity is functionally load-bearing, though purity performance (modesty spectacle, commemorative deliverance rituals) carries a real performative share. Accessibility collapse is 0.65 — inside the divine-command frame, alternatives collapse toward apostasy, but secular frames remain visible and reachable, so collapse is partial rather than mountain-grade. Resistance is 0.60: secularization waves, intermarriage rates, internal reform movements, and scholarly critique constitute sustained, recurring opposition.
 *   
 *   CYCLICAL PATTERN: the series oscillate inversely across nine shared time points spanning roughly twenty-seven centuries. External hostility tightens the fortress (t=0 conquest application, t=400 post-exilic intermarriage purge, t=1300 medieval communal discipline, t=2500-2700 revival-phase strictness); tolerance and emancipation loosen it (t=900 Hellenistic drift, t=2000 reform-era relaxation) and theater rises as practice becomes symbolic. The oscillation is itself a binding mechanism — intermittent reinforcement: each loosening lets members sample outside options, each re-tightening reprices what they have already begun to want, raising the felt cost of the boundary with every cycle. Base_properties values are measured at interval end (t=2700), a revival-tightened phase; a reader sampling at t=2000 would see a materially softer regime. All three tracked metrics run on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from the same text. From the authority seat the arrangement is a sacred trust it administers — coordination it did not choose but embodies, experienced as rope-like continuity. From the member seats the same structure operates as autonomy extraction under identity-locked exit. From the outsider seats it is categorical designation imposed without consultation. Same-level lateral divergence matters too: established householders and marriage-age youth hold nominally identical community status, but the rule bites hardest where desire crosses the boundary, so dependent youth are effectively trapped while established adults with marketable skills face milder effective pressure. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. The authority class sits at the beneficiary end: it collects deference and adjudication power and faces no extraction. Marriage-age members, exogamous couples, and outsiders sit at the target end: victim declarations plus powerless/trapped or designated-outsider positions drive high effective extraction, amplified for the trapped seats whose exit is identity-priced. Covenant community members are the correction case: their beneficiary declaration would derive a low d near the subsidy end, but they pay marriage autonomy, association, and submission through the same structure, and their exit is identity_locked rather than arbitrage-grade — their true position is near symmetric. The directionality_overrides entry (moderate -> 0.5) corrects the derivation for this dual-positioned seat; no other atom needs correction because the derivations land where the structure says they should.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents mislabeling in both directions. Calling this a pure snare would erase the genuine coordination content: minority identity persistence under assimilation pressure is a real collective-action problem that individual families cannot solve alone, and the mutual-aid and continuity goods are real. Calling it a pure rope would erase the expansive victim set, the autonomy extraction, and the violence-legitimation register that ordinary boundary norms lack. The founding problem (settlement-era survival against specific ancient neighbors) is partially obsolete — those neighbors are gone — but successor problems (diaspora assimilation pressure) are empirically real, hence founding_problem_status is contested rather than dead. If the contested status resolved to dead, the arrangement would persist by authority interest and inertia, and the classification should migrate toward snare or piton; the omegas route that resolution explicitly. The R5 mismatch consumer should note the pairing here is contested-status with world_rearranges, not the dead-plus-rearranges capture signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herem_kernel_reading_instantiation,
    'This constraint is one reading of kernel herem_command_dt7 — the durable_separation_reading. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three reading files: contextual_supersession_reading empties the victim set historically and lowers epsilon; allegorical_displacement_reading spiritualizes the victims and converts the behavioral mandate into introspective discipline; this reading maximizes both victim-set breadth and behavioral enforcement.',
    'The disagreement is located in the mandate''s temporal scope and the referent of ''the nations.'' Resolving the kernel toward either sibling collapses this constraint''s victim set and drops effective extraction sharply; resolving toward this reading sustains the high-extraction profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(herem_kernel_reading_instantiation, conceptual, 'Committer-frame routing: which reading of the herem kernel this constraint instantiates and what siblings would change.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (sanction machinery, economic dependence, educational closure) or internalized (members raised inside experience the boundary as reality itself, with guilt and ostracism-fear persisting after barriers are removed)?',
    'Post-exit suppression trajectory of leavers: if fear, guilt, and inability to form cross-boundary relationships persist years after physical exit, the internalized share dominates; if leavers integrate quickly, the structural share dominates.',
    'If largely internalized, effective suppression exceeds the structural measure and survives institutional reform — softening enforcement would not release members; if largely structural, enforcement reform translates directly into freed choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a high-suppression identity regime.').

omega_variable(
    contamination_threat_realism,
    'Does the expansive victim designation — all non-covenant outsiders as potential contamination threats — track a real assimilation threat, or does it construct a threat that justifies the boundary?',
    'Compare identity-persistence, retention, and welfare outcomes across minority communities with different endogamy regimes over matched generations, controlling for host-society hostility.',
    'If the threat is substantially overstated, the extraction component grows and the classification shifts snare-ward; if the threat is real, the coordination component strengthens and the rope-side reading gains weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contamination_threat_realism, empirical, 'Whether the contamination premise is empirically grounded or threat-construction.').

omega_variable(
    violence_legitimation_operativity,
    'Does the reading''s violence-legitimation register remain operable — available for invocation by authorities under stress — or is it historically inert, maintained only as text?',
    'Track invocations across crisis periods: rhetoric, policy proposals, and communal teaching in moments of perceived existential threat; compare against quiet-period treatment of the same passages.',
    'If operable, suppression and effective extraction are understated by peacetime observation and the latent coercive layer should be priced in; if inert, the register is vestigial and the regime functions as boundary discipline alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_legitimation_operativity, empirical, 'Operativity versus theatrical maintenance of the violence-legitimation register.').

omega_variable(
    timeless_claim_epistemic_status,
    'What could ground the claim that the mandate is timeless rather than historically bounded, given that the text''s composition context is historically situated and its application has oscillated for millennia?',
    'Textual-historical analysis of composition setting and reception, combined with the tradition''s own criteria for distinguishing enduring statute from situational directive.',
    'If the timelessness claim fails on the tradition''s own criteria, this reading loses its foundational axiom and the constraint collapses toward the contextual supersession sibling — emptying the standing victim set and dropping epsilon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(timeless_claim_epistemic_status, conceptual, 'Epistemic accessibility of the timelessness claim that distinguishes this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 2700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_durable_sep_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(herem_durable_sep_tr_t0, observed).
narrative_ontology:measurement(herem_durable_sep_tr_t400, herem_command_dt7__durable_separation_reading, theater_ratio, 400, 0.3).
narrative_ontology:measurement_basis(herem_durable_sep_tr_t400, observed).
narrative_ontology:measurement(herem_durable_sep_tr_t900, herem_command_dt7__durable_separation_reading, theater_ratio, 900, 0.45).
narrative_ontology:measurement_basis(herem_durable_sep_tr_t900, observed).
narrative_ontology:measurement(herem_durable_sep_tr_t1300, herem_command_dt7__durable_separation_reading, theater_ratio, 1300, 0.35).
narrative_ontology:measurement_basis(herem_durable_sep_tr_t1300, observed).
narrative_ontology:measurement(herem_durable_sep_tr_t1700, herem_command_dt7__durable_separation_reading, theater_ratio, 1700, 0.4).
narrative_ontology:measurement_basis(herem_durable_sep_tr_t1700, observed).
narrative_ontology:measurement(herem_durable_sep_tr_t2000, herem_command_dt7__durable_separation_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement_basis(herem_durable_sep_tr_t2000, observed).
narrative_ontology:measurement(herem_durable_sep_tr_t2300, herem_command_dt7__durable_separation_reading, theater_ratio, 2300, 0.42).
narrative_ontology:measurement_basis(herem_durable_sep_tr_t2300, observed).
narrative_ontology:measurement(herem_durable_sep_tr_t2500, herem_command_dt7__durable_separation_reading, theater_ratio, 2500, 0.3).
narrative_ontology:measurement_basis(herem_durable_sep_tr_t2500, observed).
narrative_ontology:measurement(herem_durable_sep_tr_t2700, herem_command_dt7__durable_separation_reading, theater_ratio, 2700, 0.28).
narrative_ontology:measurement_basis(herem_durable_sep_tr_t2700, observed).

% Extraction over time
narrative_ontology:measurement(herem_durable_sep_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement_basis(herem_durable_sep_be_t0, observed).
narrative_ontology:measurement(herem_durable_sep_be_t400, herem_command_dt7__durable_separation_reading, base_extractiveness, 400, 0.8).
narrative_ontology:measurement_basis(herem_durable_sep_be_t400, observed).
narrative_ontology:measurement(herem_durable_sep_be_t900, herem_command_dt7__durable_separation_reading, base_extractiveness, 900, 0.62).
narrative_ontology:measurement_basis(herem_durable_sep_be_t900, observed).
narrative_ontology:measurement(herem_durable_sep_be_t1300, herem_command_dt7__durable_separation_reading, base_extractiveness, 1300, 0.78).
narrative_ontology:measurement_basis(herem_durable_sep_be_t1300, observed).
narrative_ontology:measurement(herem_durable_sep_be_t1700, herem_command_dt7__durable_separation_reading, base_extractiveness, 1700, 0.7).
narrative_ontology:measurement_basis(herem_durable_sep_be_t1700, observed).
narrative_ontology:measurement(herem_durable_sep_be_t2000, herem_command_dt7__durable_separation_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement_basis(herem_durable_sep_be_t2000, observed).
narrative_ontology:measurement(herem_durable_sep_be_t2300, herem_command_dt7__durable_separation_reading, base_extractiveness, 2300, 0.6).
narrative_ontology:measurement_basis(herem_durable_sep_be_t2300, observed).
narrative_ontology:measurement(herem_durable_sep_be_t2500, herem_command_dt7__durable_separation_reading, base_extractiveness, 2500, 0.72).
narrative_ontology:measurement_basis(herem_durable_sep_be_t2500, observed).
narrative_ontology:measurement(herem_durable_sep_be_t2700, herem_command_dt7__durable_separation_reading, base_extractiveness, 2700, 0.82).
narrative_ontology:measurement_basis(herem_durable_sep_be_t2700, observed).

% Suppression requirement over time
narrative_ontology:measurement(herem_durable_sep_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(herem_durable_sep_su_t0, observed).
narrative_ontology:measurement(herem_durable_sep_su_t400, herem_command_dt7__durable_separation_reading, suppression_requirement, 400, 0.75).
narrative_ontology:measurement_basis(herem_durable_sep_su_t400, observed).
narrative_ontology:measurement(herem_durable_sep_su_t900, herem_command_dt7__durable_separation_reading, suppression_requirement, 900, 0.5).
narrative_ontology:measurement_basis(herem_durable_sep_su_t900, observed).
narrative_ontology:measurement(herem_durable_sep_su_t1300, herem_command_dt7__durable_separation_reading, suppression_requirement, 1300, 0.8).
narrative_ontology:measurement_basis(herem_durable_sep_su_t1300, observed).
narrative_ontology:measurement(herem_durable_sep_su_t1700, herem_command_dt7__durable_separation_reading, suppression_requirement, 1700, 0.72).
narrative_ontology:measurement_basis(herem_durable_sep_su_t1700, observed).
narrative_ontology:measurement(herem_durable_sep_su_t2000, herem_command_dt7__durable_separation_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement_basis(herem_durable_sep_su_t2000, observed).
narrative_ontology:measurement(herem_durable_sep_su_t2300, herem_command_dt7__durable_separation_reading, suppression_requirement, 2300, 0.58).
narrative_ontology:measurement_basis(herem_durable_sep_su_t2300, observed).
narrative_ontology:measurement(herem_durable_sep_su_t2500, herem_command_dt7__durable_separation_reading, suppression_requirement, 2500, 0.74).
narrative_ontology:measurement_basis(herem_durable_sep_su_t2500, observed).
narrative_ontology:measurement(herem_durable_sep_su_t2700, herem_command_dt7__durable_separation_reading, suppression_requirement, 2700, 0.8).
narrative_ontology:measurement_basis(herem_durable_sep_su_t2700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the herem command' decomposes into three epsilon-invariant readings of kernel herem_command_dt7. This file (durable_separation_reading) carries the high-extraction profile: timeless behavioral mandate, expansive victim set, violence-legitimation register. The upstream sibling contextual_supersession_reading shares the literal referent but bounds the mandate historically; the downstream sibling allegorical_displacement_reading displaces the referent entirely into typology. Each story has its own epsilon, beneficiaries, victims, and classification; the family linkage enables contamination propagation analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
