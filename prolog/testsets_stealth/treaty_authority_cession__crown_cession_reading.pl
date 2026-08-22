% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__crown_cession_reading, []).

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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Crown Cession Reading of the Treaty of Waitangi (English Text Controls; Full Sovereignty Ceded)
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The crown cession reading holds that the Treaty of Waitangi's English
 *   text controls its meaning, that kāwanatanga transferred full sovereignty
 *   to the Crown, and that the treaty therefore completed a legal cession:
 *   Māori customary authority is extinguished or subordinate to Crown law,
 *   and land alienation under Crown-derived title is legitimate. This was the
 *   operative constitutional doctrine from 1840, hardened by the colonial
 *   courts (Wi Parata v Bishop of Wellington, 1877, treating the treaty as
 *   non-justiciable while sovereignty stood complete), and it remains the
 *   official legal position today even as the Waitangi Tribunal's historical
 *   findings directly contest its foundation. This file authors ONLY this
 *   reading, assessed by its own lights, as one ε-invariant constraint; the
 *   sibling readings of the same kernel are separate files linked through the
 *   network. Claim/metric independence: the claimed type (rope) is what this
 *   reading believes is structurally true — a lawful constitutional
 *   coordination, protection exchanged for allegiance. The metrics are what
 *   this reading can descriptively acknowledge from its own seat, including
 *   the enforcement arc it narrates as ordinary governance and the resistance
 *   record it narrates as rebellion. The ε referent is the standing
 *   arrangement under contest — Crown sovereignty, subordinated customary
 *   authority, alienated land — assessed by this reading's lights, which is
 *   why ε is low here while the sibling files, assessing the same referent by
 *   their lights, will author it high.
 *
 * KEY AGENTS:
 *   - crown_government: agenda-setter (institutional/arbitrage) — receives sovereignty, radical title, and land revenue; maintains the frame; concedes Article Two breaches while holding the foundation non-justiciable
 *   - colonial_judiciary: agenda-setter (institutional/constrained) — authored the operative doctrine (English text controls; sovereignty non-justiciable) from Wi Parata onward
 *   - british_settlers: beneficiary (powerful/mobile) — receive legal order, alienable land title, and political control of the assembly
 *   - maori_tribal_communities: payer with secondary beneficiary position (organized/identity_locked) — bear authority subordination and land alienation; formally receive subjecthood and protection
 *   - non_signatory_iwi: excluded (organized/trapped) — bound by a completed cession they never assented to, with no seat in the frame
 *   - native_land_court: beneficiary (institutional/constrained) — jurisdiction exists only under the reading's title premise; operationalizes alienation
 *   - waitangi_tribunal: observer (institutional/analytical) — inquires into breaches; barred from the cession question; its 2014 findings contest the foundation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.26).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.3).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Crown Cession Reading of the Treaty of Waitangi (English Text Controls; Full Sovereignty Ceded)").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, 'be0e091f-b025-4db0-bf31-552e79c79e25').
narrative_ontology:cs_kernel_codification('be0e091f-b025-4db0-bf31-552e79c79e25', fixed_text).
narrative_ontology:cs_authority_grounding('be0e091f-b025-4db0-bf31-552e79c79e25', lineage).
narrative_ontology:cs_interpretation_layer_present('be0e091f-b025-4db0-bf31-552e79c79e25').
narrative_ontology:cs_reading_relation('be0e091f-b025-4db0-bf31-552e79c79e25', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('be0e091f-b025-4db0-bf31-552e79c79e25', treaty_authority_cession__retrospective_snare_exposure, forecloses).
narrative_ontology:cs_axiom('be0e091f-b025-4db0-bf31-552e79c79e25', foundational, english_text_controls_treaty_meaning).
narrative_ontology:cs_axiom_status(english_text_controls_treaty_meaning, holdable).
narrative_ontology:cs_axiom_grounding('be0e091f-b025-4db0-bf31-552e79c79e25', english_text_controls_treaty_meaning, conventional).
narrative_ontology:cs_axiom('be0e091f-b025-4db0-bf31-552e79c79e25', foundational, kawanatanga_completes_full_sovereignty_cession).
narrative_ontology:cs_axiom_status(kawanatanga_completes_full_sovereignty_cession, holdable).
narrative_ontology:cs_axiom_grounding('be0e091f-b025-4db0-bf31-552e79c79e25', kawanatanga_completes_full_sovereignty_cession, conventional).
narrative_ontology:cs_axiom('be0e091f-b025-4db0-bf31-552e79c79e25', secondary, customary_authority_subordinate_to_crown_law).
narrative_ontology:cs_axiom_status(customary_authority_subordinate_to_crown_law, holdable).
narrative_ontology:cs_axiom_grounding('be0e091f-b025-4db0-bf31-552e79c79e25', customary_authority_subordinate_to_crown_law, conventional).
narrative_ontology:cs_reference_frame('be0e091f-b025-4db0-bf31-552e79c79e25', completed_cession_undivided_crown_sovereignty).
narrative_ontology:cs_drift_state('be0e091f-b025-4db0-bf31-552e79c79e25', post_tribunal_northland_report, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('be0e091f-b025-4db0-bf31-552e79c79e25', '2026-08-05T00:00:00Z').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, british_settlers).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, maori_tribal_communities).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_tribal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, native_land_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proclaims and maintains the constitutional order grounded in the English text of the treaty: it holds that kāwanatanga transferred full sovereignty, legislates for all persons, holds radical title from which all land grants derive, and collected the revenue of land sales and preemption. Since 1975 it has funded settlements and issued formal apologies for breaches of the Article Two guarantees while maintaining that the foundation itself — completed cession — is settled law and not open to review in any forum.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Authored the operative form of the reading. From Wi Parata v Bishop of Wellington (1877) onward, the courts held the English text controlling, treated the sovereignty question as non-justiciable, and declined to enforce the treaty's guarantees against the Crown, holding customary title defeasible at the Crown's pleasure. Departing from this doctrine would require overturning more than a century of precedent from inside the frame the doctrine itself constitutes.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, colonial_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Arrived under the arrangement by choice and could leave by choice. They receive a single legal order, secure and alienable land title acquired through purchase and later through the individualization of customary title, and — from the 1850s — political control of the elected assembly that sets immigration, land, and war policy. Their land access depends on the premise that customary tenure is alienable and subordinate to Crown grant.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, british_settlers, beneficiary,
    powerful, biographical, mobile, national).

% Iwi and hapū, the overwhelming majority of whom signed the Māori text. Under this reading their kāwanatanga transferred in full and the Article Two guarantees of rangatiratanga over lands and taonga are enforceable only at the Crown's discretion. They bear the costs on the reading's own terms — authority transferred, land alienated through purchase and the Native Land Court — while formally receiving British subjecthood and Crown protection. Their land and authority are constitutive of who they are; there is no exit from the Crown's legal order, and their own account of what they assented to has no forum.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_tribal_communities, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, maori_tribal_communities, beneficiary).

% Iwi that did not sign at all, or whose territories lay outside effective Crown control in 1840. The reading binds them anyway: sovereignty proclaimed over the whole country, cession treated as complete. Their position — that they never ceded anything to anyone — would be decisive if heard, but the adjudicating framework declares the sovereignty question closed and non-justiciable, so there is no seat from which they could make it.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, non_signatory_iwi, excluded,
    organized, generational, trapped, regional).

% Established in 1865 to convert customary communal title into individualized, alienable title. Its entire jurisdiction exists because the reading places ultimate title in the Crown and treats customary tenure as defeasible; it operationalizes the premise that land alienation is legitimate, and its judges, officers, and associated legal professions draw their institutional standing from that premise.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, native_land_court, beneficiary,
    institutional, biographical, constrained, national).

% A standing commission of inquiry into treaty breaches, reporting with recommendations the Crown may decline to follow. Its statute expressly excludes the sovereignty question from its jurisdiction, so it can find breaches of the guarantees but cannot reach the foundation. Its 2014 Te Paparahi o Te Raki (Northland) report concluded that the chiefs who signed in February 1840 did not cede sovereignty — a state-body finding that contests the reading's foundation while the Tribunal's own remit stops short of it.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates a plurality of rangatira authorities into a single Crown legal order: one law, one framework for land transactions and dispute resolution, and a uniform exchange of allegiance for protection and subject status, replacing inter-hapū diplomacy and warfare with centralized adjudication.
% TRANSFER_FUNCTION: Moves legislative sovereignty and radical title from iwi and hapū to the Crown; moves land from customary communal tenure through individualized title to settler and state ownership; moves formal protection, subject status, and the Article Three rights of British subjects to Māori in exchange.
% ABSENT_VOICES: The chiefs whose understanding of kāwanatanga was governance under their own authority rather than transfer of sovereignty, and the iwi that never signed, would object that their assent was never given to what the English text claims. They are absent because the adjudicating frame declares the English text controlling and the sovereignty question non-justiciable: there is no forum in which their reading of the kernel can be heard, and even the inquiry body that could hear the guarantees is statutorily barred from the foundation.
% DISAPPEARANCE_RATIONALE: If the cession reading were abandoned overnight — English text no longer controlling, kāwanatanga no longer full sovereignty — every land title, every statute, and every exercise of state authority in New Zealand would rest on a foundation the state's own inquiry body has found defective. The constitutional order, the courts' jurisdiction, the title system, and the settlement framework would all require re-founding; nothing in the current arrangement survives the reading's disappearance intact.
% FOUNDING_PROBLEM: Securing a lawful constitutional basis for British annexation of territories already governed by rangatira authority, and an orderly, legally defensible mechanism for transferring land to settlers.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and the courts attest liveness: sovereignty perpetually requires its foundation, and they hold that foundation settled. Against that, the Waitangi Tribunal — a state body, but outside the seats that collect the arrangement's gains — found in Te Paparahi o Te Raki (2014) that the founding assent never occurred as the English text claims, and Māori legal scholarship and the sibling readings attest the founding problem was misframed from the outset. No attesting body is fully external to the arrangement: every institution capable of corroborating either account is itself constituted by the arrangement whose foundation is in question.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__crown_cession_reading_tests).
:- end_tests(treaty_authority_cession__crown_cession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε is low (0.26 at interval end) because the referent — the standing Crown-sovereignty arrangement — is assessed by this reading's own lights: the reading deems the cession valid and the resulting order lawful governance, conceding only the Article Two breaches it has formally settled since 1975. Suppression (0.30 end-state) reflects the enforcement burden the reading acknowledges as legitimate law enforcement; its series arcs up through the New Zealand Wars and the Wi Parata-era judicial foreclosure of customary title, peaks in the assimilation era of full land-court operation, then declines after 1975 as the reading partially accommodates contestation through the Tribunal and settlement machinery. Theater (0.25) is modest but rising: the reading persists as official law while its social and historical foundation erodes, so an increasing share of its maintenance is restatement and commemoration rather than live adjudication. Resistance (0.70) is descriptively unavoidable — the arrangement met sustained armed and political resistance across the entire interval; the reading classifies it as rebellion but cannot deny its scale or duration. Accessibility collapse (0.60): within the legal order the alternative — Māori authority as of right — is foreclosed as non-justiciable, but it never collapsed as a social and political position, which is why the value sits well below natural-law levels. All three tracked series share one time grid (1840, 1860, 1877, 1900, 1930, 1960, 1985, 2014, 2025).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (crown_government, colonial_judiciary) and the payer seat (maori_tribal_communities) should compute different types from the same structural data. From the Crown seat the arrangement is a lawful founding it built and maintains, with conceded and remedied breaches; from the identity-locked payer seat the identical structure is the subordination of its authority and the alienation of its land, experienced as wrongs its own account of 1840 never assented to. The excluded seat (non_signatory_iwi) is starker still: bound by a transaction it never entered. The engine computes this divergence per seat; the reading's own claim does not and cannot adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   crown_government sits near the beneficiary end: it receives sovereignty, radical title, and land revenue, and it controls the frame that defines what the treaty means (d low, amplified by arbitrage-grade exit — it can amend, interpret, and settle around the constraint). british_settlers are beneficiaries with mobile exit: they arrived under the arrangement by choice and their costs are diffuse (d low). maori_tribal_communities are declared beneficiaries of protection in the reading's own frame but structurally bear the transfer — authority and land — and their identity_locked exit pushes them toward the full-target end despite the beneficiary declaration (d high); the dual beneficiary/victim declaration encodes exactly this dual position. non_signatory_iwi are excluded rather than coordinated: bound by the cession without assenting, their exclusion is part of what the frame maintains. native_land_court benefits derivatively — its jurisdiction exists only under the reading's premise — and waitangi_tribunal observes from a remit that stops short of the foundation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a lawful basis for annexation and orderly land transfer — is contested rather than dead: the reading holds it live because sovereignty perpetually requires its foundation, while the Tribunal's findings and the sibling readings hold it was never validly constituted. Authoring the reading as a rope claim with honestly acknowledged enforcement, resistance, and conceded breaches prevents two mislabels. It stops the arrangement being read as pure extraction from the reading's own seat, which would erase the genuine coordination function the reading claims (a single legal order, protection, subjecthood) and the transfers it actually delivered on its own terms. And it stops the reading's self-presentation as settled coordination from being accepted at face value by the payer seat, whose computed classification will diverge sharply. The status (contested) × disappearance_verdict (world_rearranges) combination is the live signal: the arrangement is indispensable to the existing world AND its founding problem is disputed by the state's own inquiry body — precisely the configuration in which capture and zombie dynamics must be checked rather than assumed, and in which the rising theater_ratio series matters as the leading indicator of performative maintenance replacing live function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the crown_cession_reading of the treaty_authority_cession kernel; what changes structurally if a sibling reading is instantiated instead?',
    'Instantiate rangatiratanga_retention_reading and retrospective_snare_exposure as separate constraint files over the same referent and compare per-seat classifications, ε, and victim sets across the family.',
    'Under the retention reading the wall becomes a partnership with ongoing consent requirements, redistributing beneficiaries and victims and raising ε sharply. Under the snare-exposure reading the mistranslation itself is the mechanism, the 1840 signatories join the victim set, and ε reaches its family maximum. The disagreement is located in exactly two structural elements: which text controls, and what kāwanatanga transferred.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: which reading of the kernel this constraint instantiates and what the sibling readings would change.').

omega_variable(
    text_control_defensibility,
    'Is the premise that the English text controls defensible when roughly 540 chiefs signed the Māori text and about 39 the English?',
    'Comparative treaty-interpretation doctrine (contra proferentem against the party that supplied the text), combined with the historical record of what explanations were actually given at each signing and what the interpreters conveyed.',
    'If contra proferentem or the signing-pattern evidence prevails, the cession reading''s textual foundation collapses and ε re-authors sharply upward in this file''s own lights. If the courts'' conventional acceptance suffices, the reading stands on positivist grounds independent of textual fairness — the constraint survives, but its grounding migrates from the kernel to state practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_control_defensibility, empirical, 'Whether English-text control survives interpretive doctrine and the signing record.').

omega_variable(
    intertemporal_cession_validity,
    'Does nineteenth-century international law — the intertemporal principle and the recognition record — independently validate Crown sovereignty even if treaty assent to the English sovereignty claim was defective?',
    'International-law scholarship applying the intertemporal doctrine to 1840 New Zealand, together with the recognition record: Letters Patent, Colonial Office instruments, and foreign recognition of British sovereignty.',
    'If yes, the cession reading survives on non-treaty grounds and the constraint''s foundation migrates from the kernel to state practice, changing which omega governs its persistence. If no, the arrangement lacks any valid foundation and the reading''s legitimacy rests entirely on enforcement — which would reclassify this file''s own metrics from its own seat, not just from the siblings''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intertemporal_cession_validity, empirical, 'Whether cession validity can be rescued by era-contemporaneous international law rather than the treaty text.').

omega_variable(
    official_persistence_under_contestation,
    'Can the cession reading persist indefinitely as official law while the state''s own inquiry body has found the founding cession did not occur as the English text claims?',
    'Track whether the sovereignty question remains excluded from every justiciable forum, whether settlement practice expands into constitutional recognition (co-governance, rangatiratanga-recognizing instruments), and whether any forum ever admits the cession question.',
    'If persistence continues with the question forever unadjudicable, the reading''s maintenance grows increasingly theatrical relative to its contested foundation and theater_ratio becomes the leading indicator of piton-like drift. If a forum opens, the constraint faces re-founding or replacement and the family''s classifications recompute wholesale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(official_persistence_under_contestation, empirical, 'Persistence dynamics of an official reading whose foundation is contested by the state''s own inquiry body.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crown_cession_reading_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(crown_cession_reading_tr_t1860, treaty_authority_cession__crown_cession_reading, theater_ratio, 1860, 0.08).
narrative_ontology:measurement(crown_cession_reading_tr_t1877, treaty_authority_cession__crown_cession_reading, theater_ratio, 1877, 0.1).
narrative_ontology:measurement(crown_cession_reading_tr_t1900, treaty_authority_cession__crown_cession_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(crown_cession_reading_tr_t1930, treaty_authority_cession__crown_cession_reading, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(crown_cession_reading_tr_t1960, treaty_authority_cession__crown_cession_reading, theater_ratio, 1960, 0.13).
narrative_ontology:measurement(crown_cession_reading_tr_t1985, treaty_authority_cession__crown_cession_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(crown_cession_reading_tr_t2014, treaty_authority_cession__crown_cession_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement(crown_cession_reading_tr_t2025, treaty_authority_cession__crown_cession_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(crown_cession_reading_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.1).
narrative_ontology:measurement(crown_cession_reading_be_t1860, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1860, 0.13).
narrative_ontology:measurement(crown_cession_reading_be_t1877, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1877, 0.16).
narrative_ontology:measurement(crown_cession_reading_be_t1900, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(crown_cession_reading_be_t1930, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1930, 0.18).
narrative_ontology:measurement(crown_cession_reading_be_t1960, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1960, 0.19).
narrative_ontology:measurement(crown_cession_reading_be_t1985, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(crown_cession_reading_be_t2014, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2014, 0.25).
narrative_ontology:measurement(crown_cession_reading_be_t2025, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2025, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(crown_cession_reading_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.15).
narrative_ontology:measurement(crown_cession_reading_su_t1860, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1860, 0.55).
narrative_ontology:measurement(crown_cession_reading_su_t1877, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1877, 0.6).
narrative_ontology:measurement(crown_cession_reading_su_t1900, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(crown_cession_reading_su_t1930, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(crown_cession_reading_su_t1960, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(crown_cession_reading_su_t1985, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement(crown_cession_reading_su_t2014, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2014, 0.32).
narrative_ontology:measurement(crown_cession_reading_su_t2025, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2025, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi is a single contested kernel (treaty_authority_cession) whose colloquial label covers structurally distinct claims; per the ε-invariance principle it decomposes into three constraint files rather than one story with a measurement parameter. This file is the crown_cession_reading: English text controls, kāwanatanga equals full sovereignty, cession complete — ε low by its own lights. The sibling rangatiratanga_retention_reading authors the same standing arrangement as retained-sovereignty partnership (high ε by its lights); the sibling retrospective_snare_exposure authors the textual divergence itself as the mechanism (highest ε, signatories in the victim set). This reading is the upstream member: it was operative first, and its enforcement (courts declaring the sovereignty question non-justiciable; the Tribunal's statutory bar) created the legitimacy conditions under which the downstream readings were excluded from adjudicable forums. Each file links the others; family comparison of per-seat classifications is the measurement the decomposition exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
