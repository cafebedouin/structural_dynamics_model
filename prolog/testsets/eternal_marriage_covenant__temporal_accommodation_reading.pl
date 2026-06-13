% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Temporal Accommodation of Eternal Marriage Covenant (Manifesto Reading)
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   In 1890, the LDS Church leadership declared (in the 'Manifesto') that the
 *   church would 'cease the practice of contracting any marriages forbidden
 *   by the law of the land.' This statement suspended *polygamous practice*
 *   while explicitly NOT renouncing the doctrine that eternal celestial
 *   marriage (the covenant structure in D&C 132) remains binding eternally.
 *   The temporal-accommodation reading interprets the Manifesto as a
 *   *temporary* suspension necessitated by federal political pressure — the
 *   doctrine sleeps but is not dead; it will be restored when legal
 *   constraints lift. This reading contrasts sharply with the
 *   immutable-commandment reading (D&C 132 is eternal and should never be
 *   suspended) and the prophetic-override reading (the living prophet can
 *   supersede prior revelation when revelation changes). The
 *   temporal-accommodation reading occupies the pragmatic middle: the eternal
 *   law is real, but obedience to temporary secular law takes precedence
 *   until the founding problem (legal conflict) is resolved. The measuring
 *   period (1880–1920) captures the pre-Manifesto escalation, the 1890 pivot,
 *   and the subsequent institutional stabilization. Theater ratio spikes at
 *   the Manifesto point (0.58) because the constraint's primary function
 *   shifts from actual regulation of marriage practice to *managing the
 *   doctrinal claim itself* — sustaining the narrative that suspension is not
 *   repudiation.
 *
 * KEY AGENTS:
 *   - lds_church_institutional: The agenda-setter; maintains the suspension and the doctrinal claim simultaneously
 *   - federal_government: The payer; applies the pressure that makes suspension necessary
 *   - plural_practitioners: Beneficiaries of the reading's assertion that their covenants are not invalidated, even though practice is suspended; identity-locked to the eternal doctrine
 *   - monogamous_members: Beneficiaries of institutional survival and statehood gains
 *   - anti_polygamy_reformers: Excluded; would argue for full repudiation, not suspension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.31).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.42).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, scaffold).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Temporal Accommodation of Eternal Marriage Covenant (Manifesto Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:has_sunset_clause(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '90b1c859-171c-471d-8a44-b80a63a6a486').
narrative_ontology:cs_kernel_codification('90b1c859-171c-471d-8a44-b80a63a6a486', fixed_text).
narrative_ontology:cs_authority_grounding('90b1c859-171c-471d-8a44-b80a63a6a486', lineage).
narrative_ontology:cs_interpretation_layer_present('90b1c859-171c-471d-8a44-b80a63a6a486').
narrative_ontology:cs_reading_relation('90b1c859-171c-471d-8a44-b80a63a6a486', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('90b1c859-171c-471d-8a44-b80a63a6a486', eternal_marriage_covenant__prophetic_override_reading, influences).
narrative_ontology:cs_axiom('90b1c859-171c-471d-8a44-b80a63a6a486', foundational, eternal_covenant_legitimacy_persists_under_practice_suspension).
narrative_ontology:cs_axiom_status(eternal_covenant_legitimacy_persists_under_practice_suspension, holdable).
narrative_ontology:cs_axiom_grounding('90b1c859-171c-471d-8a44-b80a63a6a486', eternal_covenant_legitimacy_persists_under_practice_suspension, deontological).
narrative_ontology:cs_axiom('90b1c859-171c-471d-8a44-b80a63a6a486', foundational, obedience_to_civil_law_temporarily_supersedes_eternal_covenant_practice).
narrative_ontology:cs_axiom_status(obedience_to_civil_law_temporarily_supersedes_eternal_covenant_practice, holdable).
narrative_ontology:cs_axiom_grounding('90b1c859-171c-471d-8a44-b80a63a6a486', obedience_to_civil_law_temporarily_supersedes_eternal_covenant_practice, conventional).
narrative_ontology:cs_reference_frame('90b1c859-171c-471d-8a44-b80a63a6a486', eternal_covenant_temporarily_constrained).
narrative_ontology:cs_drift_state('90b1c859-171c-471d-8a44-b80a63a6a486', contemporary_legal_settlement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('90b1c859-171c-471d-8a44-b80a63a6a486', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, lds_church_institutional).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, plural_practitioners_during_suspension).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, monogamous_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, federal_government).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, divine_law_superiority_to_human_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The LDS Church hierarchy declares the Manifesto (1890) suspending polygamous practice while maintaining that the underlying eternal covenant remains doctrinally valid. The institutional seat faces federal pressure — statehood denial, property seizures, criminal prosecution of leaders — and chooses suspension as a survival strategy that preserves the doctrinal claim for future restoration. Administers the constraint through stake and temple systems; defines what 'obedience to law of land' requires in practice.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, lds_church_institutional, agenda_setter,
    institutional, civilizational, constrained, national).

% Applies legal and political pressure to eliminate polygamy, conditioning statehood on cessation. Enforces through prosecution and asset seizure. From the temporal-accommodation reading, the federal pressure is the temporary constraint that necessitates suspension — it is not permanent law in the theological framework, but temporary political reality.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, payer,
    institutional, generational, mobile, national).

% Church members who held plural marriages prior to 1890 or who wished to continue the practice. The Manifesto does NOT dissolve existing plural families or declare prior polygamy invalid — it suspends *future* practice. Those in active plural arrangements navigate legal exposure while maintaining their theological conviction that their covenant remains eternally binding. They benefit from the reading's assertion that their covenants are not erased, merely temporarily dormant.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, plural_practitioners_during_suspension, beneficiary,
    moderate, biographical, identity_locked, local).

% Church members (and the growing membership base post-1890) for whom monogamy is the only available or acceptable marriage form. They benefit from the settlement itself — statehood, social integration, institutional survival — even though the doctrinal claim to eternal polygamy remains unrecanted. Their practical marriage options remain unchanged; they are not the targets of the suspension.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, monogamous_members, beneficiary,
    organized, generational, constrained, national).

% Religious and civic reform movements that opposed polygamy as immoral and un-American. They view the Manifesto as a victory of law and conscience over a perverse doctrine. They are excluded from the constraint's framing — they would argue the eternal marriage covenant itself is false and should be formally repudiated, not merely suspended. Their position is not accommodated in the temporal-accommodation reading.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, anti_polygamy_reformers, excluded,
    organized, biographical, analytical, national).

% Historians, theologians, and scholars (both inside and outside the LDS tradition) who analyze the Manifesto. They examine whether suspension genuinely preserves the eternal covenant or whether it functionally repudiates it. They hold no institutional power but their interpretations influence how the constraint is understood by subsequent generations.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, theological_commentators, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__temporal_accommodation_reading, lds_church_institutional).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__temporal_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves institutional conflict between a revealed religious doctrine (eternal celestial marriage) and secular law (prohibition of polygamy) by suspending the practice while maintaining the doctrinal claim, enabling the church to meet political conditions for statehood and legal recognition without formally abandoning its foundational cosmology.
% TRANSFER_FUNCTION: Transfers authority to adjudicate the eternal law from the living prophet (who could, under continuing-revelation reading, supersede D&C 132 entirely) to a framing that treats federal law as temporarily constraining the *practice* while the doctrine remains binding in principle. Plural practitioners give up *future* practice (but not past covenants or doctrinal validity) in exchange for institutional survival and eventual restoration rights.
% ABSENT_VOICES: Anti-polygamy reformers and those who view the eternal marriage covenant doctrine itself as false/immoral are explicitly excluded — they would object that suspension is insufficient and that the covenant should be formally repudiated. Dissenting church members who believed the Manifesto betrayed the foundational doctrine (some of whom continued polygamist branches) are also excluded from the official constraint narrative.
% DISAPPEARANCE_RATIONALE: If the temporal-accommodation reading vanished — if the LDS Church formally renounced the eternal marriage covenant doctrine entirely — the institutional survival gains (statehood, legal standing) would remain, but the reading's particular way of managing the theological tension (suspension vs. repudiation) would be gone. The contest is whether the reading's disappearance would *restore* the prior state (a living eternal covenant needing only political opportunity) or *complete* a repudiation the Manifesto already began. Protagonists of the immutable-commandment reading say it would restore; prophetic-override reading proponents say it would complete. The temporal-accommodation reading itself holds that disappearance would be a decision point — not a restoration of prior equilibrium.
% FOUNDING_PROBLEM: The LDS Church's foundational doctrine (D&C 132) established plural marriage as an eternal, required covenant, while U.S. federal law criminalized polygamy and conditioned statehood on its cessation. By 1890, the conflict was unresolvable by doctrine alone — the church faced institutional extinction (territory denial, prosecution, asset seizure) if it continued. The founding problem is a pure collision between two authority systems.
% FOUNDING_PROBLEM_CORROBORATION: U.S. federal law successfully criminalized polygamy and no longer actively prosecutes it (the founding problem of legal conflict is resolved). However, anti-polygamy reformers and secular commentators attest that the founding problem was resolved by the church's capitulation, not by compromise — the doctrine was not genuinely suspended, merely abandoned in practice while maintained in theory for institutional convenience. Church historians and theologians outside the LDS tradition (e.g., Kathleen Flake, D. Michael Quinn) document that the Manifesto was a strategic survival move, not a theological innovation. The corroboration is split: institutional sources treat the founding problem as resolved; critical scholars treat it as symptomatically unresolved (the reading persists because the underlying doctrinal claim persists, even though the legal conflict is moot).
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, contested).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).
:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.31 at endpoint) because the constraint does not produce rent collection or pure asymmetric gain — it manages a doctrinal-legal collision. The church extracts compliance from practitioners (suspend polygamy) and silence from dissenters (don't formally repudiate the doctrine), but the gain is institutional survival, not surplus accumulation. Suppression is moderate (0.42) because enforcement relies heavily on institutional loyalty and theological commitment — practitioners *choose* to obey the Manifesto because they accept the reading's frame (eternal law temporarily constrained), not purely from coercion. The theater ratio peaks at 1890 (0.58) and remains elevated because the constraint's operation is heavily performative: the church must *appear* to have abandoned polygamy while *sustaining* the doctrinal claim. This simultaneity is the theatrical core — maintaining two contradictory narratives at once. Accessibility alternatives do not wholly collapse (0.67) because legal marriage outside polygamy remains available and because the reading itself acknowledges that secular law legitimately constrains practice. Resistance is moderate (0.54) because the immanent tension between suspension and doctrine generates persistent low-level contestation — dissidents who reject the reading form breakaway groups.
 *
 * PERSPECTIVAL GAP:
 *   The institutional seat (LDS leadership) experiences the constraint as a necessary theological maneuver: suspension preserves the eternal covenant while satisfying federal pressure. Plural practitioners experience it as a temporary sacrifice — their covenants are theologically valid, merely dormant. Anti-polygamy reformers (excluded) experience it as an evasion: the doctrine itself should be repudiated. The engine computes these divergences from the power/exit structure: the institution has mobile exit (it could in principle move jurisdiction or abandon the doctrine), plural practitioners are identity-locked (their self-concept depends on the eternal covenant's validity), and reformers are trapped (they must work through institutional and legal channels to enforce repudiation). From each seat, the constraint's type diverges.
 *
 * DIRECTIONALITY LOGIC:
 *   The LDS church institutional seat sits near the beneficiary end (d ≈ 0.2): it survives institutional extinction, gains statehood, and controls the interpretation of what suspension means. Plural practitioners sit in a complex position (d ≈ 0.45): they sacrifice current practice but gain doctrinal validation of their past covenants and future restoration rights; the reading benefits them more than a full repudiation would, but less than continuing polygamy would. Federal government sits as a payer (d ≈ 0.75): it bears the cost of negotiating the settlement and monitoring compliance, but succeeds in its core objective (elimination of legal polygamy). Monogamous members and future generations benefit from institutional stability without bearing practice costs (d ≈ 0.1). The reading's directional structure is asymmetric: institutional leadership controls the frame, practitioners are constrained to accept it, federal government achieves its objective at cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal conflict between doctrine and law) is factually DEAD by 1920 — federal law has criminalized polygamy effectively, statehood has been granted, and the conflict is no longer acute. However, the constraint persists because the reading's entire logic depends on the founding problem remaining theoretically unresolved: the suspension is temporary *only if* the founding problem might someday lift. Once the founding problem is definitively moot (federal law will not change, statehood is secure), the suspension's rationale becomes ambiguous. The constraint is vulnerable to reclassification as mandatrophic: what was *supposed* to be a temporary scaffold (suspension pending resolution of legal conflict) becomes an indefinite inertial structure because the resolution it was built for has already occurred. The theater ratio's persistence at elevated levels (0.60 by 1920) signals this: the constraint's primary function has shifted from managing a real legal conflict to maintaining a doctrinal narrative. This is not Mandatrophy Resolved (the founding problem is solved, but the solution does not restore prior equilibrium — it entrenches the suspension indefinitely), but a trajectory toward Piton classification as the doctrinal claim loses institutional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_repudiation_ambiguity,
    'Does the Manifesto''s suspension of polygamous practice constitute a genuine preservation of the eternal covenant doctrine, or does it functionally repudiate the doctrine even while maintaining the theological claim?',
    'Analyze the LDS Church''s post-1890 doctrinal development: do authoritative church sources treat the eternal marriage covenant as available for future restoration, or has the doctrine been quietly retired while the formal claim persists? The 1998 and subsequent official statements of church doctrine would be the primary evidence.',
    'If the doctrine is genuinely preserved for future restoration, the reading''s scaffold classification is justified (temporary suspension, eventual restoration). If the doctrine has been functionally abandoned (only maintained for historical consistency), the constraint reclassifies as Piton (inertial maintenance of a dormant claim) or false-summit-mountain (a natural law that is actually benefiting the institution by appearing inevitable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_vs_repudiation_ambiguity, empirical, 'Whether the eternal covenant remains a live doctrinal commitment or is a historical claim sustained for institutional inertia.').

omega_variable(
    theological_authority_source_ambiguity,
    'In the temporal-accommodation reading, what is the source of authority that legitimizes suspending the eternal covenant? Is it the secular law itself (obedience to law of land takes precedence), or is it a theological principle (a revelation or continuing revelation that authorizes the suspension)?',
    'Examine the rhetorical and theological grounding of the Manifesto and subsequent LDS statements: do they justify the suspension by reference to divine command (God authorizes the suspension), by reference to pragmatic necessity, or by reference to the binding nature of secular law? The shift between these grounds would indicate whether the reading is theologically or pragmatically grounded.',
    'If the suspension is grounded in divine command, the reading is a form of prophetic override (contrary to the sibling prophetic_override_reading, which claims the prophet can fully supersede D&C 132). If it is grounded in pragmatic necessity or the binding nature of secular law, the reading is more exposed to reclassification as Snare (coercive suspension masked as doctrinal accommodation) or Tangled_Rope (genuine coordination between religious and secular authority). The authority source determines whether the constraint is defensible as religious commitment or vulnerable as institutional expedience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_authority_source_ambiguity, conceptual, 'The theological vs. pragmatic grounding of the suspension doctrine.').

omega_variable(
    identity_lock_persistence_boundary,
    'For plural practitioners bound by the temporal-accommodation reading, what would it take for the identity-lock to break? Does the reading contain an expiration date or restoration condition (e.g., ''if federal law changes, practice resumes''), or is the lock indefinite?',
    'Analyze the Manifesto''s language and LDS statements about the suspension''s duration: are there explicit or implicit conditions under which the suspension would lift? Has the LDS Church ever stated that the eternal covenant can be definitively abandoned (removing the restoration option), or does the theoretical possibility of restoration remain?',
    'If the reading maintains a genuine restoration condition, plural practitioners'' identity-lock is justified (the covenant remains eternally valid, merely dormant). If no restoration condition exists and the lock is indefinite, practitioners are trapped in a doctrinal claim without exit — the constraint becomes extractive from their perspective (they sacrifice practice indefinitely without prospect of restoration). This affects classification of the payer seat (plural practitioners): are they temporarily constrained agents, or permanently trapped ones?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_boundary, empirical, 'Whether the identity-lock binding plural practitioners is temporary or indefinite.').

omega_variable(
    committer_reading_forelosure_test,
    'Does the temporal-accommodation reading foreclose the immutable-commandment reading, or do they coexist as live positions within LDS theology?',
    'Test the logical structure: the immutable-commandment reading asserts D&C 132 must never be suspended (it is eternally binding in practice). The temporal-accommodation reading asserts it can be suspended temporarily. These positions are not formally contradictory — one could hold that the covenant is immutable in principle but temporarily suspended in practice. However, if the temporal-accommodation reading is adopted as institutional doctrine and plural practice is explicitly forbidden by institutional authority, does that foreclose the immutable-commandment reading as a live position within the church? Or do adherents of the immutable reading form dissenting branches (as they historically did)?',
    'If coexists_with, the readings remain live options within or alongside the LDS tradition. If forecloses, the institutional adoption of temporal accommodation has ruled out the immutable reading as tenable within official church bounds. The relation type determines how the constraint affects institutional coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_forelosure_test, conceptual, 'Whether the temporal accommodation reading logically rules out the immutable commandment reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1880, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1880, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1880, 0.25).
narrative_ontology:measurement_basis(eter_tr_t1880, observed).
narrative_ontology:measurement(eter_tr_t1888, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1888, 0.35).
narrative_ontology:measurement_basis(eter_tr_t1888, observed).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.58).
narrative_ontology:measurement_basis(eter_tr_t1890, observed).
narrative_ontology:measurement(eter_tr_t1895, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1895, 0.62).
narrative_ontology:measurement_basis(eter_tr_t1895, observed).
narrative_ontology:measurement(eter_tr_t1910, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1910, 0.64).
narrative_ontology:measurement_basis(eter_tr_t1910, observed).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1920, 0.6).
narrative_ontology:measurement_basis(eter_tr_t1920, observed).

% Extraction over time
narrative_ontology:measurement(eter_be_t1880, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1880, 0.18).
narrative_ontology:measurement_basis(eter_be_t1880, observed).
narrative_ontology:measurement(eter_be_t1888, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1888, 0.42).
narrative_ontology:measurement_basis(eter_be_t1888, observed).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.31).
narrative_ontology:measurement_basis(eter_be_t1890, observed).
narrative_ontology:measurement(eter_be_t1895, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1895, 0.28).
narrative_ontology:measurement_basis(eter_be_t1895, observed).
narrative_ontology:measurement(eter_be_t1910, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1910, 0.25).
narrative_ontology:measurement_basis(eter_be_t1910, observed).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1920, 0.22).
narrative_ontology:measurement_basis(eter_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1880, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1880, 0.15).
narrative_ontology:measurement_basis(eter_su_t1880, observed).
narrative_ontology:measurement(eter_su_t1888, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1888, 0.55).
narrative_ontology:measurement_basis(eter_su_t1888, observed).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.42).
narrative_ontology:measurement_basis(eter_su_t1890, observed).
narrative_ontology:measurement(eter_su_t1895, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1895, 0.38).
narrative_ontology:measurement_basis(eter_su_t1895, observed).
narrative_ontology:measurement(eter_su_t1910, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1910, 0.35).
narrative_ontology:measurement_basis(eter_su_t1910, observed).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1920, 0.32).
narrative_ontology:measurement_basis(eter_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__temporal_accommodation_reading, 0.12).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__prophetic_override_reading).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel decomposes into three structurally distinct constraint readings: immutable_commandment_reading (D&C 132 is eternal, should never be suspended; Mountain or Tangled_Rope), temporal_accommodation_reading (suspension is temporary, pending legal/political resolution; Scaffold), and prophetic_override_reading (continuing revelation allows formal supersession; Tangled_Rope or Snare). Each reading has its own ε, beneficiary/victim structure, and type. They are related via kernel-reading family links: all three are readings of the same foundational doctrine, but they instantiate different constraints because they disagree on whether suspension is possible, whether it is justified, and whether it is permanent. The three stories must be linked via network.affects_constraints to enable contamination analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__temporal_accommodation_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
