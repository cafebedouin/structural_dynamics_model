% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Classical Expansionist-Legalist Reading of Jihad (Siyar Doctrine)
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   This story authors the expansionist-legalist reading of jihad within the
 *   classical siyar (Islamic law of nations) tradition: jihad as an
 *   obligation, resting with the imam/caliph alone, to extend Islamic
 *   governance through campaigns conducted under formal conditions — prior
 *   invitation (da'wa) to accept Islam or dhimmi status, proportionality in
 *   conduct, and non-combatant protections — while explicitly permitting
 *   offensive initiation against polities that have not accepted these terms.
 *   This is a distinct constraint from the defensive-spiritual reading (which
 *   denies the offensive-legitimacy premise entirely) and from the
 *   revolutionary-vanguard reading (which relocates the obligation from state
 *   authority to the individual believer via takfir). The three readings
 *   share a textual kernel — the Quranic corpus and associated hadith on
 *   qital and jihad — but instantiate structurally different constraints with
 *   different victim sets, different authority loci, and different epsilon
 *   values; per the ε-invariance principle they are authored as three
 *   separate files linked by network edges, not as one story with a contested
 *   interpretation flag.
 *
 * KEY AGENTS:
 *   - caliph_or_imam: sets war/peace policy, institutional/arbitrage — administers the doctrine and collects its political yield
 *   - classical_jurist_class: articulates the conditions that legitimate campaigns, institutional — collects interpretive authority and patronage
 *   - conquered_dhimmi_subjects and non_muslim_populations_outside_dar_al_islam: bear the doctrine's costs, powerless/trapped
 *   - comparative_legal_historians: analytical observer of the doctrine's textual and historical operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.62).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.68).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Classical Expansionist-Legalist Reading of Jihad (Siyar Doctrine)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '1dcc0d81-b690-4556-8fd6-a9a50dce9041').
narrative_ontology:cs_kernel_codification('1dcc0d81-b690-4556-8fd6-a9a50dce9041', fixed_text).
narrative_ontology:cs_authority_grounding('1dcc0d81-b690-4556-8fd6-a9a50dce9041', lineage).
narrative_ontology:cs_interpretation_layer_present('1dcc0d81-b690-4556-8fd6-a9a50dce9041').
narrative_ontology:cs_reading_relation('1dcc0d81-b690-4556-8fd6-a9a50dce9041', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('1dcc0d81-b690-4556-8fd6-a9a50dce9041', jihad_quranic_corpus__revolutionary_vanguard_reading, influences).
narrative_ontology:cs_axiom('1dcc0d81-b690-4556-8fd6-a9a50dce9041', foundational, offensive_campaign_permissible_under_rule_bound_conditions).
narrative_ontology:cs_axiom_status(offensive_campaign_permissible_under_rule_bound_conditions, holdable).
narrative_ontology:cs_axiom_grounding('1dcc0d81-b690-4556-8fd6-a9a50dce9041', offensive_campaign_permissible_under_rule_bound_conditions, conventional).
narrative_ontology:cs_axiom('1dcc0d81-b690-4556-8fd6-a9a50dce9041', foundational, war_declaration_vested_exclusively_in_imam_authority).
narrative_ontology:cs_axiom_status(war_declaration_vested_exclusively_in_imam_authority, overridden).
narrative_ontology:cs_axiom_grounding('1dcc0d81-b690-4556-8fd6-a9a50dce9041', war_declaration_vested_exclusively_in_imam_authority, conventional).
narrative_ontology:cs_reference_frame('1dcc0d81-b690-4556-8fd6-a9a50dce9041', classical_siyar_caliphal_authority).
narrative_ontology:cs_drift_state('1dcc0d81-b690-4556-8fd6-a9a50dce9041', post_caliphate_abolition_1924, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('1dcc0d81-b690-4556-8fd6-a9a50dce9041', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_state_apparatus).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurist_class).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_polity_treasury).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations_outside_dar_al_islam).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, conquered_dhimmi_subjects).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, frontier_populations_under_recurring_campaign).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_soldiery_and_volunteers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, muslim_soldiery_and_volunteers).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_monopoly_on_just_war_declaration).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, territorial_expansion_as_religious_duty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds sole legal authority to declare offensive jihad, negotiate terms of invitation (da'wa) to Islam, set proportionality rules, and distribute spoils (ghanima) and land revenue (kharaj) from conquered territory. The doctrine vests war-making power exclusively in this office, converting territorial expansion into a lawful, revenue-generating state function.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliph_or_imam, agenda_setter,
    institutional, generational, arbitrage, continental).

% Articulates and administers the conditions (invitation, proportionality, treatment of prisoners, dhimmi contracts) that make the doctrine operate as law rather than raw conquest. Their interpretive monopoly over what counts as valid invitation, valid proportionality, and valid authority is itself a source of durable institutional standing and patronage from the state.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurist_class, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurist_class, agenda_setter).

% Receives the fifth (khums), kharaj land tax, and jizya poll tax generated by successful campaigns and subsequent administration of conquered territory. Systematic expansion under this doctrine is a recurring revenue mechanism, not merely a security response.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_polity_treasury, beneficiary,
    institutional, generational, arbitrage, continental).

% Non-Muslim populations who submit after conquest are absorbed into protected-but-subordinate legal status: exempted from military obligation but taxed (jizya), barred from certain public displays of religion, and excluded from political authority. Exit means conversion, emigration where permitted, or perpetual second-class legal standing; none is cost-free.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, conquered_dhimmi_subjects, payer,
    powerless, generational, trapped, regional).

% Communities not yet under Islamic governance are, under this reading, legitimately subject to invitation followed by lawful offensive campaign if the invitation is refused. Their prior sovereignty is not treated as a bar to the campaign; the doctrine converts their non-Muslim governance itself into the triggering condition for war, however proportionate the jurists insist the campaign must be.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations_outside_dar_al_islam, payer,
    powerless, biographical, trapped, continental).

% Border regions between dar al-islam and dar al-harb experience repeated seasonal campaigns (e.g. historical Byzantine frontier raiding) justified under the same doctrine, bearing recurring destruction, displacement, and tribute demands as a structural feature of the frontier rather than a one-time event.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, frontier_populations_under_recurring_campaign, payer,
    powerless, biographical, trapped, regional).

% Participants receive shares of spoils and religious merit for participation, but bear the mortal risk of campaign and are bound by the imam's authorization — individual soldiers cannot legitimately freelance jihad under this reading, only fight under sanctioned command, which both empowers and constrains them relative to the vanguard reading's individual-obligation model.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_soldiery_and_volunteers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, muslim_soldiery_and_volunteers, payer).

% Contemporary actors who invoke this classical doctrine to justify state action or territorial claims operate in a world of settled international borders and non-existent caliphal authority; they are not part of the classical juristic conversation that produced the doctrine's conditions, and their invocation strips out the very authority-monopoly condition (imam authority) the doctrine requires, a tension not addressed within this reading itself.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, modern_islamic_states_and_movements, excluded,
    organized, generational, constrained, global).

% Study the doctrine's textual sources, its historical instantiation across dynasties, and its divergence from both the defensive-spiritual and revolutionary-vanguard readings, without a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, diffuse).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rule-bound framework that channels warfare away from unrestricted raiding into a legally regulated activity with declared authority, required prior invitation, proportionality limits, and defined post-conquest status for the defeated — coordinating what would otherwise be arbitrary violence into an administered, treaty-like process with predictable legal consequences (dhimmi status, taxation, property rules).
% TRANSFER_FUNCTION: Moves political sovereignty, tax revenue (kharaj, jizya), and movable wealth (ghanima) from non-Muslim polities and populations to the caliphal treasury, the jurist class administering the legal apparatus, and participating soldiery, conditioned on the formal legal process (invitation, authorization, proportionality) being observed.
% ABSENT_VOICES: The populations targeted for invitation-then-campaign have no standing within the doctrine to contest the legitimacy of being classified as a valid target; their prior sovereignty and consent are not questions the doctrine's own jurisprudence entertains. Contemporary Muslim-majority states operating in a caliphate-less, treaty-bound international order are also absent from the framework that presupposes their central authority structure.
% DISAPPEARANCE_RATIONALE: If this reading's authority ceased to organize state practice, the historical mechanism converting territorial non-Muslim governance into a lawful casus belli would vanish; frontier populations would no longer face doctrinally sanctioned recurring campaigns, the caliphal treasury would lose kharaj/jizya as a war-linked revenue stream, and the jurist class's authority over declaring valid causes for offensive war would need an entirely different legitimating basis or would dissolve.
% FOUNDING_PROBLEM: Early Islamic polity needed a legal framework to regulate warfare being conducted by an expanding state — replacing tribal raiding norms with rules governing when war could be initiated, how it must be conducted, who could authorize it, and what legal status resulted for the defeated, while also expressing a theological mandate to extend Islamic governance.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists (al-Shafi'i, al-Sarakhsi) and historical caliphal chroniclers attest the doctrine functioned as both genuine legal regulation of state violence and a live expansionist mandate during the early conquests. Modern comparative legal historians and many contemporary Muslim scholars attest the doctrine's offensive-war component addressed a specific historical and political condition (absence of settled international law, expanding Muslim polity) that no longer obtains under the modern state system — corroboration from outside the beneficiary class (i.e., not from caliphal or jurist-class sources) supports the 'founding problem substantially resolved, doctrine potentially retained for other purposes' reading.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the doctrine's operation transfers sovereignty, tax revenue, and wealth from conquered non-Muslim populations to the caliphal state and jurist class, conditioned on but not eliminated by the formal legal process. Suppression (0.68) is higher than extraction because the doctrine's persistence as state practice depended on the caliph's monopoly over declaration and the coercive apparatus of conquest and administration — populations under this doctrine had no legal standing to contest their classification as valid targets. Theater ratio is moderate-low (0.28): the jurisprudential conditions (invitation, proportionality) were substantively adjudicated in classical legal literature, not purely decorative, but their observance in actual campaign practice varied and is itself a live historical dispute, which the theater ratio partially captures. Accessibility collapse (0.5) and resistance (0.6) are moderate rather than mountain-level: this is a constructed legal-political doctrine, not a natural law — alternatives (non-expansionist readings, treaty-based coexistence) existed and were actively argued within the same tradition, and conquered/targeted populations mounted real resistance, both militarily and through eventual treaty renegotiation.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliph/imam and jurist class sit at the beneficiary end: they set the terms, administer the process, and collect its yield (political authority, tax revenue, interpretive prestige) without bearing its costs. Conquered dhimmi subjects and non-Muslim populations outside dar al-islam sit at the target end: they are structurally trapped (their governance status is precisely what triggers the doctrine's operation) and bear taxation, subordinate legal status, or campaign destruction. Muslim soldiery occupies an intermediate position — beneficiaries of spoils and religious merit, but bearing mortal risk and bound to follow authorized command rather than act independently, which is itself a directionality-relevant constraint distinguishing this reading from the vanguard reading's individual-obligation model.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a genuine mandatrophy candidate: the doctrine was built to regulate an expanding early-Islamic polity's warfare in the absence of any settled international legal order, and to express a theological mandate for that expansion. Modern comparative legal-historical corroboration (attestation from outside the beneficiary class) supports treating the state-formation/international-order component of the founding problem as substantially resolved by the emergence of a settled state system with mutually recognized borders, while the underlying theological expansion mandate remains a live but contested claim within Islamic jurisprudence itself. This is precisely the kind of divergence the disappearance-verdict/founding-problem-status mismatch is designed to surface, rather than resolve by fiat: status is authored 'contested' because Muslim jurists themselves are divided on whether the offensive component survives the disappearance of caliphal authority, while the disappearance verdict is 'world_rearranges' because the doctrine, where invoked, still organizes real transfers and real subordinate-status arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    offensive_versus_defensive_textual_priority,
    'Do the Quranic verses and hadith cited for this reading (e.g. Quran 9:5, 9:29, and associated qital verses) establish a general offensive mandate conditioned by jurisprudential rules, or are they properly read as bounded to specific historical circumstances of active hostility against the early Muslim community, as the defensive-spiritual reading holds?',
    'Comparative philological and historical-contextual (asbab al-nuzul) analysis of the relevant verses, cross-referenced against classical exegetical traditions and their internal disputes, would not fully resolve this given genuine scholarly disagreement, but would narrow the range of textually defensible readings.',
    'If the offensive reading cannot be textually sustained independent of specific historical circumstance, this reading''s claimed_type and beneficiary structure would need reassessment as a historically contingent legal innovation rather than a standing doctrinal obligation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(offensive_versus_defensive_textual_priority, conceptual, 'Whether the offensive-permission premise is textually general or historically bounded — the central contest between this reading and the defensive-spiritual sibling.').

omega_variable(
    authority_monopoly_survivability,
    'Given that no functioning caliphate has existed since 1924, does this reading''s imam-authority-monopoly condition mean the doctrine is currently inoperative in its own terms, or do modern state structures inherit the caliph''s authorizing function?',
    'Examination of contemporary fatwas and state practice by Muslim-majority governments claiming or disclaiming this authority, and scholarly consensus (or its absence) on state succession to caliphal jurisdiction over war declaration.',
    'If no successor authority is recognized, invocations of this doctrine by modern non-state actors are, by this reading''s own jurisprudential logic, structurally invalid — a finding that would sharply narrow (but not eliminate) contemporary application while distinguishing this reading further from the vanguard reading''s explicit authority-bypass.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_monopoly_survivability, empirical, 'Whether the caliph/imam authority condition has a living referent after 1924.').

omega_variable(
    proportionality_practice_gap,
    'To what extent did historical campaigns conducted under this doctrine actually observe the jurisprudential conditions (prior invitation, proportionality, non-combatant immunity) versus using them as post-hoc legal cover for conquest already underway?',
    'Historical record analysis comparing jurist-recorded conditions against campaign chronicles and treaty records across multiple dynasties (Umayyad, Abbasid, Ottoman) to establish observance rates.',
    'A high compliance-gap would raise the effective theater_ratio and support reclassifying portions of the doctrine''s historical operation closer to snare (extraction with legal cover) rather than tangled_rope (genuine but asymmetric coordination); a low gap would support the coordination function as substantively real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_practice_gap, empirical, 'Whether the doctrine''s stated legal conditions were substantively observed or mainly rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 632, 1924).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t632, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 632, 0.15).
narrative_ontology:measurement_basis(jiha_tr_t632, observed).
narrative_ontology:measurement(jiha_tr_t750, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 750, 0.2).
narrative_ontology:measurement_basis(jiha_tr_t750, observed).
narrative_ontology:measurement(jiha_tr_t1000, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1000, 0.24).
narrative_ontology:measurement_basis(jiha_tr_t1000, observed).
narrative_ontology:measurement(jiha_tr_t1250, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1250, 0.26).
narrative_ontology:measurement_basis(jiha_tr_t1250, observed).
narrative_ontology:measurement(jiha_tr_t1500, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1500, 0.27).
narrative_ontology:measurement_basis(jiha_tr_t1500, observed).
narrative_ontology:measurement(jiha_tr_t1750, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1750, 0.28).
narrative_ontology:measurement_basis(jiha_tr_t1750, observed).
narrative_ontology:measurement(jiha_tr_t1924, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1924, 0.28).
narrative_ontology:measurement_basis(jiha_tr_t1924, observed).

% Extraction over time
narrative_ontology:measurement(jiha_be_t632, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 632, 0.45).
narrative_ontology:measurement_basis(jiha_be_t632, observed).
narrative_ontology:measurement(jiha_be_t750, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 750, 0.55).
narrative_ontology:measurement_basis(jiha_be_t750, observed).
narrative_ontology:measurement(jiha_be_t1000, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement_basis(jiha_be_t1000, observed).
narrative_ontology:measurement(jiha_be_t1250, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1250, 0.6).
narrative_ontology:measurement_basis(jiha_be_t1250, observed).
narrative_ontology:measurement(jiha_be_t1500, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1500, 0.6).
narrative_ontology:measurement_basis(jiha_be_t1500, observed).
narrative_ontology:measurement(jiha_be_t1750, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1750, 0.62).
narrative_ontology:measurement_basis(jiha_be_t1750, observed).
narrative_ontology:measurement(jiha_be_t1924, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1924, 0.62).
narrative_ontology:measurement_basis(jiha_be_t1924, observed).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t632, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 632, 0.5).
narrative_ontology:measurement_basis(jiha_su_t632, observed).
narrative_ontology:measurement(jiha_su_t750, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 750, 0.6).
narrative_ontology:measurement_basis(jiha_su_t750, observed).
narrative_ontology:measurement(jiha_su_t1000, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1000, 0.63).
narrative_ontology:measurement_basis(jiha_su_t1000, observed).
narrative_ontology:measurement(jiha_su_t1250, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1250, 0.65).
narrative_ontology:measurement_basis(jiha_su_t1250, observed).
narrative_ontology:measurement(jiha_su_t1500, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1500, 0.66).
narrative_ontology:measurement_basis(jiha_su_t1500, observed).
narrative_ontology:measurement(jiha_su_t1750, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1750, 0.68).
narrative_ontology:measurement_basis(jiha_su_t1750, observed).
narrative_ontology:measurement(jiha_su_t1924, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1924, 0.68).
narrative_ontology:measurement_basis(jiha_su_t1924, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposed from the jihad_quranic_corpus kernel per the ε-invariance principle: the defensive_spiritual_reading (jihad as internal struggle plus constrained defensive response — low extraction, near-rope), this expansionist_legalist_reading (jihad as rule-bound offensive state obligation — moderate-high extraction, tangled_rope), and the revolutionary_vanguard_reading (jihad as individual duty bypassing state authority via takfir — likely high extraction and suppression given its authority-bypass mechanism, closer to snare). Each reading has a distinct beneficiary/victim structure and a distinct epsilon; they are linked here rather than merged because measuring 'jihad' by different observables (internal-spiritual practice vs. state war-powers doctrine vs. individual-obligation emergency jurisprudence) yields incommensurable extraction values — exactly the signal that triggers decomposition rather than a single parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
