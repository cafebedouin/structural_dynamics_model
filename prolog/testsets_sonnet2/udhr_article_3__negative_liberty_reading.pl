% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: Article 3 as Negative Liberty: State Non-Deprivation Reading
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the negative-liberty reading of UDHR Article 3:
 *   the state's obligation is exhausted by refraining from arbitrary killing
 *   and detention, with 'security' defined as freedom FROM state violence
 *   rather than freedom from violence generally. Under this reading, capital
 *   punishment is abolitionist-incompatible, self-defense doctrine (including
 *   state use of lethal force) is read restrictively, and due process
 *   protections expand to cover nearly every deprivation scenario. This is a
 *   genuine coordination achievement — a shared anti-atrocity floor across
 *   jurisdictions — but it also transfers real costs onto crime victims,
 *   communities facing organized non-state violence, and states trying to
 *   administer collective security, none of whom consented to have 'security'
 *   defined exclusively in state-restraint terms. The coordination/extraction
 *   hybrid is why this reading computes as tangled_rope rather than pure
 *   rope: courts and rights bodies (agenda-setters) coordinate a genuine
 *   anti-tyranny floor while communities under gang or insurgent violence
 *   (payers) absorb the cost of a doctrine indifferent to non-state threats.
 *
 * KEY AGENTS:
 *   - criminal_defendants: primary beneficiary (powerless/trapped) — protected by narrow procedural justice requirement
 *   - political_dissidents: primary beneficiary (powerless/constrained) — protected from extrajudicial state violence
 *   - civil_liberties_organizations: agenda-setter (organized/mobile) — authors and litigates the doctrinal architecture
 *   - communities_facing_organized_violence: primary payer (powerless/trapped) — bears cost of restrictive self-defense doctrine amid non-state threats
 *   - states_administering_collective_security: payer/agenda-setter (institutional/constrained) — bound by the doctrine while administering public safety
 *   - international_human_rights_bodies: agenda-setter (institutional/analytical) — enforces compliance globally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.58).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "Article 3 as Negative Liberty: State Non-Deprivation Reading").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, '24ebcd1f-409f-421a-b623-904154f38ce6').
narrative_ontology:cs_kernel_codification('24ebcd1f-409f-421a-b623-904154f38ce6', fixed_text).
narrative_ontology:cs_authority_grounding('24ebcd1f-409f-421a-b623-904154f38ce6', lineage).
narrative_ontology:cs_interpretation_layer_present('24ebcd1f-409f-421a-b623-904154f38ce6').
narrative_ontology:cs_reading_relation('24ebcd1f-409f-421a-b623-904154f38ce6', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('24ebcd1f-409f-421a-b623-904154f38ce6', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('24ebcd1f-409f-421a-b623-904154f38ce6', foundational, security_is_absence_of_state_coercion).
narrative_ontology:cs_axiom_status(security_is_absence_of_state_coercion, holdable).
narrative_ontology:cs_axiom_grounding('24ebcd1f-409f-421a-b623-904154f38ce6', security_is_absence_of_state_coercion, deontological).
narrative_ontology:cs_axiom('24ebcd1f-409f-421a-b623-904154f38ce6', foundational, procedural_justice_exhausts_state_obligation_under_article_3).
narrative_ontology:cs_axiom_status(procedural_justice_exhausts_state_obligation_under_article_3, holdable).
narrative_ontology:cs_axiom_grounding('24ebcd1f-409f-421a-b623-904154f38ce6', procedural_justice_exhausts_state_obligation_under_article_3, conventional).
narrative_ontology:cs_reference_frame('24ebcd1f-409f-421a-b623-904154f38ce6', post_totalitarian_anti_atrocity_settlement).
narrative_ontology:cs_drift_state('24ebcd1f-409f-421a-b623-904154f38ce6', post_cold_war_human_rights_jurisprudence_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24ebcd1f-409f-421a-b623-904154f38ce6', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, criminal_defendants).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, civil_liberties_organizations).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, crime_victims_seeking_deterrence).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, communities_facing_organized_violence).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, states_administering_collective_security).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face state prosecutorial power and rely on the negative-liberty reading to bar execution, arbitrary detention, and abbreviated process. The reading's insistence on narrow procedural justice is their primary shield against the state's coercive apparatus; they have no exit from the jurisdiction and depend entirely on the doctrine holding.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, criminal_defendants, beneficiary,
    powerless, biographical, trapped, national).

% Depend on the restrictive reading of state authority to prevent extrajudicial killing, disappearance, and detention without trial. The doctrine's narrow-procedural-justice requirement is what stands between them and state violence when they challenge government power.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, political_dissidents, beneficiary,
    powerless, biographical, constrained, national).

% Litigate, lobby, and author the doctrinal architecture that reads Article 3 as pure negative liberty — abolition of capital punishment, restrictive self-defense doctrine, expansive due process. They set the interpretive agenda through litigation strategy, treaty commentary, and international jurisprudence, and are largely insulated from the security costs the reading imposes.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, civil_liberties_organizations, agenda_setter,
    organized, generational, mobile, global).

% Bear the cost when the negative-liberty reading forecloses capital punishment and constrains preventive detention regardless of victim-impact considerations. They have no standing in the doctrinal debate and no exit from a legal system that has already resolved the question against retributive or incapacitative measures.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, crime_victims_seeking_deterrence, payer,
    powerless, biographical, trapped, local).

% Live under gang, cartel, or insurgent violence where the restrictive self-defense doctrine and expansive due process protections for suspects limit the state's capacity for preventive or forceful intervention. The reading treats state restraint as the paramount good regardless of the community's exposure to non-state violence; they cannot relocate away from the threat or opt out of the legal framework that constrains response.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, communities_facing_organized_violence, payer,
    powerless, biographical, trapped, regional).

% Must administer public safety, counter-terrorism, and emergency powers under a doctrine that treats nearly every deviation from narrow procedural justice as illegitimate deprivation. They retain some rule-making capacity (constrained exit) but are bound by treaty commitments, constitutional courts, and international monitoring bodies that enforce the negative-liberty reading against derogation.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, states_administering_collective_security, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, states_administering_collective_security, agenda_setter).

% Issue interpretive commentary, adjudicate individual complaints, and monitor state compliance under the negative-liberty framework. They enforce the reading through reporting mechanisms, advisory opinions, and diplomatic pressure, holding states to the abolitionist and restrictive-force standard regardless of local security conditions.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, international_human_rights_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% Argue that Article 3's security guarantee is empty without material provision — freedom from state violence means little to someone dying of preventable poverty. They are structurally excluded from this reading's frame, which treats their claim as a category error (welfare is not what Article 3 obligates) rather than engaging it on the merits.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, positive_entitlement_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__negative_liberty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared floor against state violence: courts, prosecutors, and legislatures across many jurisdictions converge on a common minimum — no execution, no arbitrary killing, no detention without narrow procedural justification — enabling predictable cross-border human rights adjudication and a stable baseline for asylum, extradition, and treaty compliance.
% TRANSFER_FUNCTION: Moves discretionary coercive capacity away from the state and toward the individual subject to state power: prosecutorial and security discretion is transferred into procedural constraints, and the political cost of maintaining capital punishment or aggressive security measures is transferred onto states and onto communities who bear the consequences of constrained state response to non-state violence.
% ABSENT_VOICES: Crime victims, communities under organized-violence threat, and positive-entitlement advocates are structurally marginalized in this reading's forums (constitutional courts, UN treaty bodies) which are dominated by civil-liberties litigators and international jurists; their objections are treated as security-panic or category confusion rather than engaged as competing readings of the same text.
% DISAPPEARANCE_RATIONALE: If the negative-liberty reading vanished overnight, states retaining capital punishment or expanded security detention would lose their primary international legal exposure; extradition and asylum law built on non-refoulement to death penalty jurisdictions would collapse; civil liberties litigation strategy built on this doctrinal architecture would need wholesale reconstruction; and communities facing organized violence might see expanded state force with fewer procedural checks.
% FOUNDING_PROBLEM: Post-1948 drafters sought to prevent recurrence of state-perpetrated mass killing, arbitrary execution, and disappearance as practiced by totalitarian and colonial regimes — the founding trauma was state violence against populations with no procedural recourse.
% FOUNDING_PROBLEM_CORROBORATION: Human rights historians and the drafting record (travaux préparatoires) corroborate the anti-totalitarian founding purpose as still substantially live in authoritarian and quasi-authoritarian states. However, criminologists and security-policy scholars outside the civil-liberties advocacy community argue the doctrine has drifted from preventing state atrocity toward constraining ordinary democratic security policy — a shift they attest is not corroborated by the founding record, which targeted extraordinary state violence, not routine law enforcement or counter-terrorism operations.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.35 to 0.68) tracking the doctrinal expansion from a narrow anti-atrocity floor to a broad due-process and abolitionist regime that increasingly constrains ordinary security administration, not just extraordinary state violence. Suppression (0.58) reflects the active enforcement machinery — treaty monitoring, constitutional court supremacy, extradition non-refoulement doctrine — that makes deviation from this reading costly for states. Theater ratio is comparatively low (0.22): the doctrine does substantive work (executions are actually barred, detention actually reviewed), it is not primarily performative. Resistance is high (0.72) because states, security agencies, and victim-advocacy groups actively contest the doctrine's scope, particularly its application to counter-terrorism and organized crime contexts.
 *
 * PERSPECTIVAL GAP:
 *   Civil liberties organizations and international human rights bodies experience this reading as pure coordination — a hard-won floor against state atrocity that should not be weakened by security exceptions. Communities facing organized violence and states administering collective security experience the same structure as extraction: a rigid doctrine that ties their hands against non-state threats while treating any deviation as illegitimate. The engine should compute these as structurally different seats because the beneficiary seats have low, insulated exposure to the costs the doctrine imposes on the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Criminal defendants and political dissidents sit near the full-beneficiary end: the doctrine's entire purpose, from their position, is to shield them from state power, and they bear none of the collective-security costs. Civil liberties organizations and international bodies are structural agenda-setters with mobile/analytical exit — they author and enforce the reading without personally bearing its security costs. Crime victims and organized-violence-affected communities sit near the full-target end: trapped, local/regional scope, no standing in the interpretive forums, and directly exposed to the doctrine's restriction on state response capacity. States administering collective security are a partial target: institutional power gives them some capacity to negotiate derogations, but treaty commitments and constitutional review constrain that capacity substantially.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing totalitarian-style state atrocity) remains partially live in authoritarian contexts, which is why founding_problem_status is 'contested' rather than 'dead' — this classification prevents mislabeling the entire doctrine as pure extraction. But the tangled_rope classification also prevents mislabeling the doctrine's contemporary application to ordinary democratic security policy as pure coordination: the same doctrinal machinery built to stop state massacres now also constrains routine counter-terrorism and organized-crime response, and that extension imposes costs on parties (crime victims, threatened communities) who were not the intended beneficiaries of the founding settlement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negative_liberty_kernel_selection,
    'Is the negative-liberty reading of Article 3 the textually and historically correct reading of the kernel, or does it selectively emphasize the drafting history''s anti-totalitarian concerns while suppressing the drafters'' simultaneous concern with material security (reflected in Articles 22-25''s welfare provisions)?',
    'Comparative analysis of the travaux préparatoires across all UDHR articles, examining whether the drafters treated ''security'' in Article 3 as a term of art distinct from the welfare provisions elsewhere in the document, or as part of an integrated conception later split by Cold War ideological pressure (Western states pushing negative rights, Soviet bloc pushing positive rights) into separate covenants (ICCPR vs ICESCR).',
    'If the split is shown to be a Cold War political artifact rather than a conceptual necessity in the original text, the negative_liberty_reading''s claim to be the ''natural'' or default reading weakens substantially, and its high ε becomes harder to justify as textually compelled rather than politically constructed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negative_liberty_kernel_selection, conceptual, 'Whether the negative/positive liberty split is textually grounded or a Cold War-era doctrinal artifact.').

omega_variable(
    collective_security_tradeoff_measurement,
    'Does the restrictive self-defense and due-process doctrine under this reading empirically increase harm to communities facing organized violence, or does robust procedural protection for suspects ultimately produce better security outcomes by preventing state overreach, corruption, and wrongful targeting that would otherwise undermine community trust and cooperation with law enforcement?',
    'Cross-jurisdictional comparison of violent crime and organized-violence outcomes in states with strict versus permissive interpretations of Article 3-equivalent due process protections, controlling for baseline security conditions, state capacity, and rule-of-law strength.',
    'If restrictive doctrine correlates with worse organized-violence outcomes independent of confounds, the payer classification for affected communities is strongly corroborated. If outcomes are neutral or better, the extraction claim weakens and the doctrine looks more like genuine long-run coordination even for the seemingly burdened communities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_security_tradeoff_measurement, empirical, 'Whether restrictive due-process doctrine helps or harms communities facing organized violence in practice.').

omega_variable(
    capital_punishment_deterrence_contest,
    'Does capital punishment abolition, mandated under this reading''s extension of Article 3, produce a measurable deterrence loss that harms crime victims and public safety, or is the deterrent effect of capital punishment empirically negligible such that abolition imposes no real security cost?',
    'Meta-analysis of deterrence econometrics literature across abolitionist and retentionist jurisdictions with comparable socioeconomic and law-enforcement conditions.',
    'A confirmed deterrence effect would substantiate crime_victims_seeking_deterrence as genuine victims bearing a real security cost; a null or negligible effect would suggest the extraction claim rests on a folk-theory of deterrence rather than a measurable harm, reducing this reading''s actual ε contribution from the capital punishment component specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_punishment_deterrence_contest, empirical, 'Whether capital punishment abolition under this reading imposes a measurable deterrence-loss cost on crime victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__negative_liberty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(udhr_tr_t8, udhr_article_3__negative_liberty_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(udhr_tr_t16, udhr_article_3__negative_liberty_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(udhr_tr_t24, udhr_article_3__negative_liberty_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(udhr_tr_t32, udhr_article_3__negative_liberty_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(udhr_tr_t40, udhr_article_3__negative_liberty_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__negative_liberty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(udhr_be_t8, udhr_article_3__negative_liberty_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(udhr_be_t16, udhr_article_3__negative_liberty_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(udhr_be_t24, udhr_article_3__negative_liberty_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(udhr_be_t32, udhr_article_3__negative_liberty_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(udhr_be_t40, udhr_article_3__negative_liberty_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__negative_liberty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(udhr_su_t8, udhr_article_3__negative_liberty_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(udhr_su_t16, udhr_article_3__negative_liberty_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(udhr_su_t24, udhr_article_3__negative_liberty_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(udhr_su_t32, udhr_article_3__negative_liberty_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(udhr_su_t40, udhr_article_3__negative_liberty_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating the contested udhr_article_3 kernel. negative_liberty_reading (this file) authors high ε (0.68) via capital punishment abolition, restrictive self-defense doctrine, and expansive due process, with beneficiaries = individuals against state power and victims = collective security measures and their intended beneficiaries. positive_entitlement_reading authors a structurally different constraint (state obligated to provide material conditions for life/security), with a different beneficiary/victim structure entirely (likely beneficiaries = the materially deprived, victims = taxpayers/resource-allocation losers). procedural_hybrid_reading authors a narrower, likely lower-ε constraint focused only on due-process mechanics without resolving the substantive negative/positive contest. Per the ε-invariance principle, these are three separate constraints sharing a kernel, not one constraint measured three ways — each has its own ε, beneficiaries, victims, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
