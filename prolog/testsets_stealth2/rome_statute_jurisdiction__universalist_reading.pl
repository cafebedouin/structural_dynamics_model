% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universal-Mandate Reading (Jurisdiction Transcending Consent)
 *   domain: international law / treaty interpretation / institutional authority
 *
 * SUMMARY:
 *   Under the universalist reading, the Rome Statute establishes a mandate
 *   for international criminal justice that transcends state consent:
 *   nationals of non-party states fall within the court's reach through
 *   territorial triggers (Article 12(2)(a)) and Security Council referrals
 *   (Article 13(b)), victims' claims do not depend on their government's
 *   ratification, and core-crime accountability overrides sovereign
 *   objection. The arrangement this story is about is that mandate as it
 *   actually operates — the expanding docket of situations touching
 *   non-consenting states (Libya, Comoros, Palestine, Afghanistan,
 *   Bangladesh/Myanmar, Ukraine), the warrant diplomacy directed at sitting
 *   officials, and the cooperation web that substitutes for the court's
 *   absent police power. Epsilon is authored for this standing arrangement,
 *   assessed descriptively — not discounted by this reading's endorsement of
 *   the mandate, and not measured against the sovereigntist alternative this
 *   reading contests. T indexes years since entry into force (T0 = 2002, T24
 *   = 2026). The claim/metrics split is deliberate: the constraint is CLAIMED
 *   as tangled_rope from its structure (genuine coordination function plus
 *   asymmetric extraction plus active enforcement), while the metrics
 *   describe its observed operation independently. KEY AGENTS (by structural
 *   relationship): - icc_officialdom: Agenda-setter and principal collecting
 *   seat (institutional/identity_locked) — administers the mandate and
 *   accrues authority, funding, and caseload from its expansion -
 *   atrocity_victims_and_survivors: Intended beneficiary (powerless/trapped)
 *   — receive whatever accountability the mandate delivers -
 *   nonparty_state_officials: Primary target (powerful/trapped) — exposed by
 *   territorial triggers and council referrals without ever consenting -
 *   targeted_sitting_leadership: High-visibility target (powerful/trapped) —
 *   warrants convert into isolation rather than custody -
 *   prosecuted_atrocity_perpetrators: Direct coercive target
 *   (powerless/trapped) — bear detention, trial, and sentence -
 *   unsc_permanent_members: Gatekeeper (institutional/arbitrage) — hold
 *   referral and deferral veto over the mandate they partly stand outside -
 *   party_state_governments: Dual beneficiary/payer (organized/constrained) —
 *   fund and arm the court while exposing their own nationals -
 *   host_state_referring_governments: Dual beneficiary/payer
 *   (moderate/constrained) — delegate cases they cannot prosecute, at the
 *   price of reciprocal exposure - human_rights_advocacy_networks: Secondary
 *   beneficiary (organized/mobile) — the universal frame anchors their
 *   advocacy model - sovereigntist_jurists: Excluded voice (moderate/mobile)
 *   — object from outside the interpretive community -
 *   independent_international_law_scholars: Analytical observer
 *   (moderate/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.58).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.64).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universal-Mandate Reading (Jurisdiction Transcending Consent)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international law / treaty interpretation / institutional authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '2994aeed-308d-47cd-9143-95ada38e66e1').
narrative_ontology:cs_kernel_codification('2994aeed-308d-47cd-9143-95ada38e66e1', fixed_text).
narrative_ontology:cs_authority_grounding('2994aeed-308d-47cd-9143-95ada38e66e1', lineage).
narrative_ontology:cs_interpretation_layer_present('2994aeed-308d-47cd-9143-95ada38e66e1').
narrative_ontology:cs_reading_relation('2994aeed-308d-47cd-9143-95ada38e66e1', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('2994aeed-308d-47cd-9143-95ada38e66e1', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('2994aeed-308d-47cd-9143-95ada38e66e1', foundational, core_crimes_transcend_sovereign_consent).
narrative_ontology:cs_axiom_status(core_crimes_transcend_sovereign_consent, holdable).
narrative_ontology:cs_axiom_grounding('2994aeed-308d-47cd-9143-95ada38e66e1', core_crimes_transcend_sovereign_consent, deontological).
narrative_ontology:cs_axiom('2994aeed-308d-47cd-9143-95ada38e66e1', secondary, territorial_trigger_binds_nonparty_nationals).
narrative_ontology:cs_axiom_status(territorial_trigger_binds_nonparty_nationals, holdable).
narrative_ontology:cs_axiom_grounding('2994aeed-308d-47cd-9143-95ada38e66e1', territorial_trigger_binds_nonparty_nationals, conventional).
narrative_ontology:cs_reference_frame('2994aeed-308d-47cd-9143-95ada38e66e1', rome_universal_accountability_consensus).
narrative_ontology:cs_drift_state('2994aeed-308d-47cd-9143-95ada38e66e1', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2994aeed-308d-47cd-9143-95ada38e66e1', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, atrocity_victims_and_survivors).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, human_rights_advocacy_networks).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, icc_officialdom).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, host_state_referring_governments).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, party_state_governments).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, nonparty_state_officials).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, targeted_sitting_leadership).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, prosecuted_atrocity_perpetrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, host_state_referring_governments).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, party_state_governments).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, unsc_permanent_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges, prosecutors, and registry staff of the International Criminal Court administer the Rome Statute system: they open situations, issue warrants, adjudicate admissibility, and manage the cooperation web with member states. The institution's budget, caseload, and standing grow with each jurisdictional expansion, and its officials' careers are constituted by the mandate they interpret. Dissolution or jurisdictional retrenchment is not a practical option for the institution — ending the project would end the professional world its personnel inhabit.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_officialdom, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, icc_officialdom, beneficiary).

% Survivors of massacres, sexual violence, and displacement in situations from northern Uganda to Darfur to Palestine look to the court for accountability their national systems cannot or will not deliver. They participate as witnesses and victims' representatives and receive reparations when cases succeed. Their access runs entirely through the court's reach; no comparable-scale alternative redress exists for them.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, atrocity_victims_and_survivors, beneficiary,
    powerless, biographical, trapped, regional).

% NGO coalitions and campaign organizations document atrocities, lobby governments to join and cooperate, and press the prosecutor to open new situations. The universal framing of the mandate anchors their advocacy model and funding case. Their personnel move freely between organizations and capitals and bear none of the mandate's coercive costs.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, human_rights_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% Governments of states where atrocities occurred — Uganda, the Democratic Republic of the Congo, Mali, and others — referred their own territories to the court, delegating cases against armed groups they could not defeat in court or in battle. The same delegation exposes their own officials and forces to scrutiny, and several such governments have turned publicly hostile when the court's attention rotated toward them.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, host_state_referring_governments, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, host_state_referring_governments, payer).

% The roughly 125 ratifying states fund the court through assessed contributions, execute arrest warrants on their territory, and hold a standing instrument they can aim at adversaries' officials. Ratification also exposes their own nationals to investigation, which is why some military powers negotiated protections and why withdrawal — taken by Burundi and the Philippines — remains a live although costly option.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, party_state_governments, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, party_state_governments, payer).

% The five permanent Security Council members hold referral power (exercised for Darfur and Libya) and deferral power over any situation. Three of the five are not parties to the statute, yet their officials fall within the court's reach through council referrals and territorial triggers. Their veto lets them direct the mandate at others while shielding themselves and their allies from it.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, unsc_permanent_members, payer).

% Officials and military personnel of states that never joined the statute — American, Israeli, Russian, Ethiopian, and others — become investigable when crimes occur on the territory of a party state or through a council referral. They exercised no vote over the rules that expose them, their own government's courts cannot shield them once a warrant issues, and their options reduce to litigation, diplomatic pressure, retaliation against the court, and avoidance of party-state travel.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, nonparty_state_officials, payer,
    powerful, biographical, trapped, global).

% Sitting heads of state and government facing warrants — Omar al-Bashir historically; more recently leaders indicted over Ukraine and the Palestinian territories — lose the practical ability to travel widely, host summits, and bank internationally, and face strengthened domestic opponents who cite the indictment. Arrest depends on other states' willingness, so the sanction usually arrives as isolation rather than custody.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, targeted_sitting_leadership, payer,
    powerful, biographical, trapped, national).

% Individual defendants surrendered or arrested — militia commanders, rebel leaders, former conscripts risen to command — bear the court's direct coercive force: multi-year detention in The Hague, trial, and sentences served in member-state prisons. They had no role in designing the tribunal that judges them and no appellate forum above it.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, prosecuted_atrocity_perpetrators, payer,
    powerless, biographical, trapped, regional).

% Legal scholars and practitioners committed to the consent-based architecture of the Vienna Convention argue that prosecuting non-party nationals without consent violates pacta tertiis and corrodes the treaty system's foundations. Their objections circulate in journals, advisory opinions, and government memoranda, but they hold no seat in the Assembly of States Parties or the court's chambers.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, sovereigntist_jurists, excluded,
    moderate, generational, mobile, global).

% Academic analysts of international criminal law track the court's jurisdictional rulings, cooperation record, and case selection, publishing assessments of whether its practice matches its statutory premises. They take no side in the dispute and bear none of its costs.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, independent_international_law_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__universalist_reading, icc_officialdom).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a permanent, pre-agreed adjudicative machinery for atrocity crimes: common legal definitions, a standing prosecutor, and a pooled evidentiary record, so accountability does not have to be renegotiated ad hoc after each conflict. Complementarity channels cases to national systems first, with the court as backstop where states are unable or unwilling to act.
% TRANSFER_FUNCTION: Moves adjudicative authority and coercive exposure — indictment, arrest, detention — from national control to a supranational court; moves assessed contributions and diplomatic capital from member states to the court's institutions; moves reputational and mobility costs onto indicted officials and their patrons.
% ABSENT_VOICES: Non-party great-power governments (the United States, Russia, China, India) and sovereigntist international lawyers would object that territorial-trigger jurisdiction over non-consenting states' nationals contradicts pacta tertiis and the Vienna Convention's consent architecture. They sit outside the Assembly of States Parties and the court's interpretive community — addressed by warrants and diplomatic notes rather than seated in the norm's construction.
% DISAPPEARANCE_RATIONALE: If the universal mandate vanished overnight, every pending situation resting on territorial triggers or council referrals — Palestine, Afghanistan, Bangladesh/Myanmar, Ukraine, Libya — would lose its legal basis; indicted officials would regain unrestricted travel; victim communities would lose their only supranational recourse; and accountability would revert to ad hoc tribunals requiring fresh Security Council politics for each conflict, which the Yugoslav and Rwandan precedents show arrive late, selectively, or never.
% FOUNDING_PROBLEM: Post-Cold-War impunity: the Yugoslavia and Rwanda tribunals demonstrated that case-by-case Security Council politics produce slow, selective, geographically arbitrary accountability. The 1998 Rome Conference sought a permanent standing court so that genocide, war crimes, and crimes against humanity would meet law rather than negotiated amnesty wherever domestic systems fail.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: two Security Council referrals (Darfur 2005, Libya 2011) were carried by votes including non-party states; successive General Assembly resolutions and independent commissions of inquiry request court engagement; and several objecting governments acknowledge the impunity problem itself while disputing the consent-free remedy — corroborating the founding problem, though not this reading's particular resolution of it.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the mandate genuinely delivers prosecutions (Lubanga, Ntaganda, Ongwen, al-Mahdi convictions with reparations), but its consent-free reach imposes real costs on actors who never agreed to the rules, and its reach is asymmetric — weakest against the most powerful targets. Suppression 0.64: suppression is authored as a raw structural property and is NOT scaled by power or scope (only extractiveness is scaled, by directionality and spatial scope, in the engine's computation). The coercive surface is steep for individuals — an indicted person has no legal exit, only capture, flight, or death — and moderate for states, which retain withdrawal (Burundi, Philippines) and non-membership at reputational cost. Theater_ratio 0.47: a growing share of activity is performative — symbolic warrants against unreachable officials, universality campaigns, anniversary diplomacy — while the conviction record stays thin relative to the claimed scope; the trajectory (0.22 to 0.47) tracks the widening gap between declared reach and executed arrests. Accessibility_collapse 0.35: alternatives remain visible and usable — national prosecution under complementarity, ad hoc tribunals, hybrid courts, domestic universal-jurisdiction statutes, withdrawal — so understanding the constraint does not close the option space. Resistance 0.70: sustained and organized — sanctions and visa bans against court personnel, criminalization of cooperation, African Union non-cooperation strategy, treaty withdrawals, and coordinated great-power objections; the target coalition (AU, BRICS statements) is the main reason the mandate's expansion has stalled. Identity-lock dynamics: icc_officialdom is authored identity_locked through institutional identity fusion — the organization has become its function, and dissolution or jurisdictional retrenchment is unthinkable from inside; if that frame broke, the Assembly of States Parties could amend or sunset the mandate, and the classification would shift accordingly. All three tracked series share one time grid (T0-T24 at four-year steps) so no metric row borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine derives that divergence from the structural data. From the payer seats the arrangement is experienced as pure coercion: an indicted official or a detained defendant receives no coordination dividend, only exposure — their computed type should sit at the extractive extreme. From the victim and advocate seats the same structure is an accountability machine — the closest thing to a rope they have. The dual seats (party states, referring host states) experience both faces simultaneously: a lever aimed at adversaries welded to an exposure borne by their own officials. The P5 seat experiences an instrument with a veto shield — usable against others, defensible against itself. No single seat-level verdict describes the constraint; the tangled_rope claim is the whole-structure summary of exactly this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: victims, advocacy networks, and the court's own officialdom sit near the subsidized end, with the court's institutional stake making it the seat the gains demonstrably accrue to. Victim declarations drive high directionality: non-party officials, sitting leadership, and prosecuted defendants sit near the full-target end, amplified by trapped exit — an indicted person's d approaches 1.0 regardless of personal power, which is why powerful officials and powerless defendants compute similarly high. Dual-role agents (party states, host states) derive near-symmetric d from offsetting roles and constrained exit. No directionality_overrides are authored: the derivation chain handles the hardest case, the P5 gatekeepers, through exit modulation — their arbitrage-grade veto pulls their effective d well below what their secondary payer role alone would imply, which is precisely the insulation the override mechanism would otherwise have to hand-author.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — post-Cold-War impunity that ad hoc tribunals solved only when Security Council politics permitted — is still live: atrocities in Sudan, Ethiopia, Myanmar, and Ukraine continue to outrun domestic accountability. With founding_problem_status = live and disappearance_verdict = world_rearranges, the mismatch consumer finds no zombie flag, and mandatrophy_resolved is correctly not declared. The classification discipline cuts both ways: calling this a pure snare would erase the real accountability delivered to victims and the genuine collective-action problem solved; calling it a pure rope would erase the consent-free extraction from non-parties and the selective enforcement that spares the powerful. Tangled_rope holds both facts. The live drift risk is theater: if theater_ratio crosses 0.5 while convictions stay flat, the mandate decays toward piton — a universality performed rather than practiced — and the temporal series authored here is the instrument that would date that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the rome_statute_jurisdiction kernel (universalist_reading). Which structural elements does this reading fix that its siblings vary?',
    'Comparative analysis across the three family files: victim sets, directionality structure, and epsilon under each reading.',
    'Adopting the sovereigntist_reading shrinks the victim set to consenting-states-only persons and removes non-party official exposure entirely; adopting the hybrid_complementarity_reading restores a sovereign-deference gate that lowers measured extraction. This file''s classification holds only for the universalist instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position of this story within the Rome Statute jurisdiction kernel family.').

omega_variable(
    consent_architecture_disagreement_site,
    'Where exactly do the readings diverge inside the treaty text — Articles 12(2)(a)-(b) territoriality and nationality, Article 13(b) Security Council referral, or Article 17 complementarity''s admissibility role?',
    'Textual and jurisprudential analysis identifying which provisions each reading treats as load-bearing versus peripheral.',
    'If non-party-national jurisdiction rests on a minority reading of Article 12(2)(a), the universalist constraint is narrower and less extractive than authored; if it is the settled interpretive mainstream, the full victim structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_architecture_disagreement_site, conceptual, 'Locating the structural site of inter-reading disagreement within the Statute''s consent architecture.').

omega_variable(
    enforcement_capacity_gap,
    'Is the universal mandate operative authority or declaratory aspiration, given that the court executes no arrests itself and depends wholly on state cooperation?',
    'Track warrant-execution rates by target power level over the coming decade; compare execution rates for weak-state versus great-power indictees.',
    'If execution stays near zero for powerful targets, effective extraction concentrates on weak-state defendants and the arrangement drifts toward a selective-extraction profile; robust execution would vindicate the coordination-function reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_gap, empirical, 'Whether claimed universal reach corresponds to enforceable authority.').

omega_variable(
    selectivity_structural_or_resource,
    'Does the de facto concentration of situations and defendants on African and weaker-state actors reflect structural capture by referring and funding states, or contingent resource and access limits?',
    'Compare situation-opening rates against atrocity incidence across regions, controlling for referral source and funder composition.',
    'Structural capture would sharpen the extraction asymmetry and support a snare-drift hypothesis for this reading; resource contingency would leave the tangled_rope classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_structural_or_resource, empirical, 'Source of the court''s documented case-selection asymmetry.').

omega_variable(
    complementarity_gate_character,
    'Within this reading, does Article 17 complementarity operate as genuine sovereign deference or as a thin admissibility filter controlled by the court itself?',
    'Code the court''s inability/unwillingness determinations for deference depth: how often are national proceedings accepted as sufficient versus displaced?',
    'Genuine deference lowers this reading''s effective extraction and moves it toward the hybrid sibling; rubber-stamp behavior confirms the consent-transcending character and sustains the higher extraction measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complementarity_gate_character, conceptual, 'Character of the complementarity gate inside the universalist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__universalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(rome_tr_t4, rome_statute_jurisdiction__universalist_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(rome_tr_t8, rome_statute_jurisdiction__universalist_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(rome_tr_t12, rome_statute_jurisdiction__universalist_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(rome_tr_t16, rome_statute_jurisdiction__universalist_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__universalist_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(rome_tr_t24, rome_statute_jurisdiction__universalist_reading, theater_ratio, 24, 0.47).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rome_be_t4, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(rome_be_t8, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(rome_be_t12, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(rome_be_t16, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(rome_be_t24, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(rome_su_t4, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(rome_su_t8, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(rome_su_t12, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(rome_su_t16, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(rome_su_t24, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 24, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Rome Statute jurisdiction' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the universalist reading (this file — consent-transcending mandate, epsilon 0.58, victims include non-party officials and sitting leadership), the sovereigntist reading (strictly consent-gated framework, materially lower epsilon, victim set largely empty), and the hybrid complementarity reading (conditional balance, intermediate epsilon). The upstream universalist claim supplies interpretive momentum that the hybrid reading's admissibility practice absorbs; each file links the other two as a constraint family, and each carries its own epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
