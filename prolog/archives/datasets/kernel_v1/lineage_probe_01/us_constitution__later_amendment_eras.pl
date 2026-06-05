% ============================================================================
% CONSTRAINT STORY: us_constitution__later_amendment_eras
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution__later_amendment_eras, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: us_constitution__later_amendment_eras
 *   human_readable: US Constitution as Living Amendment Record (Reconstruction-Centered)
 *   domain: political/legal/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested US
 *   Constitution kernel. This reading declares: 'The Constitution's operative
 *   meaning is set by successive waves of formal constitutional amendment,
 *   above all the Reconstruction amendments (13th, 14th, 15th), not by the
 *   founding text of 1787.' This reading locates constitutional authority in
 *   the amendment process and treats the founding text as the baseline that
 *   amendments systematically supersede and reinterpret. This is one of five
 *   structural readings in the constraint family. The other readings claim
 *   authority differently: the bill_of_rights reading locates it in 1791, the
 *   original_constitution reading insists the 1787 text is supreme, the
 *   pre_constitutional reading traces it to ante-1787 compact, and the
 *   failed_amendments reading defines it through rejection. Each reading
 *   produces a different constraint because each declares a different
 *   authority structure, different beneficiary/victim sets, and different
 *   extractiveness. This story generates ONLY the later-amendment-eras
 *   reading as a clean, ε-invariant constraint. The others are separate
 *   constraint files linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Freedmen and suffrage-expansion coalitions (moderate/constrained): Primary beneficiaries. Expanded political community formally enfranchised by amendments. Experience the living-amendment reading as the mechanism of their inclusion.
 *   - Amendment ratifying coalitions (institutional/arbitrage): Secondary beneficiaries. Political coalitions that successfully ratified amendments benefit from ratification (increased legitimacy, political base, enforceable commitments).
 *   - Defenders of original 1787 settlement and states-rights tradition (moderate/constrained, originalist interpreters): Primary victims. Their constitutional frame is formally superseded by successive amendments. Experience the living-amendment reading as suppression of their interpretive authority.
 *   - Judicial interpretive authority and living constitutionalists (institutional/arbitrage): Institutional beneficiary. Courts wielding the living-amendment reading gain interpretive discretion to update constitutional meaning without formal amendment.
 *   - Originalist and textualist interpretive communities (analytical/analytical): Intellectual counterposition. Hold that the unamended 1787 text or original public meaning, not amendment cascades, sets operative constitutional authority.
 *   - The Constitution itself as a binding commitment: Meta-agent. The constraint describes not just a political fact but a legitimacy claim about what the Constitution IS—a living amendment record or a fixed founding text.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution__later_amendment_eras, 0.52).
domain_priors:suppression_score(us_constitution__later_amendment_eras, 0.58).
domain_priors:theater_ratio(us_constitution__later_amendment_eras, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution__later_amendment_eras, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution__later_amendment_eras, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution__later_amendment_eras, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution__later_amendment_eras, tangled_rope).
narrative_ontology:human_readable(us_constitution__later_amendment_eras, "US Constitution as Living Amendment Record (Reconstruction-Centered)").
narrative_ontology:topic_domain(us_constitution__later_amendment_eras, "political/legal/constitutional_law").

domain_priors:requires_active_enforcement(us_constitution__later_amendment_eras).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution__later_amendment_eras, '74026de7-baff-4620-850d-4ecc15bf33b6').
narrative_ontology:cs_kernel_codification('74026de7-baff-4620-850d-4ecc15bf33b6', fixed_text).
narrative_ontology:cs_authority_grounding('74026de7-baff-4620-850d-4ecc15bf33b6', extraction).
narrative_ontology:cs_interpretation_layer_present('74026de7-baff-4620-850d-4ecc15bf33b6').
narrative_ontology:cs_reading_relation('74026de7-baff-4620-850d-4ecc15bf33b6', us_constitution__bill_of_rights_1791, influences).
narrative_ontology:cs_reading_relation('74026de7-baff-4620-850d-4ecc15bf33b6', us_constitution__original_constitution_1787, coexists_with).
narrative_ontology:cs_reading_relation('74026de7-baff-4620-850d-4ecc15bf33b6', us_constitution__pre_constitutional_frameworks, influences).
narrative_ontology:cs_reading_relation('74026de7-baff-4620-850d-4ecc15bf33b6', us_constitution__failed_amendments, coexists_with).
narrative_ontology:cs_axiom('74026de7-baff-4620-850d-4ecc15bf33b6', foundational, amendment_formality_supreme).
narrative_ontology:cs_axiom_status(amendment_formality_supreme, holdable).
narrative_ontology:cs_axiom_grounding('74026de7-baff-4620-850d-4ecc15bf33b6', amendment_formality_supreme, conventional).
narrative_ontology:cs_axiom('74026de7-baff-4620-850d-4ecc15bf33b6', foundational, reconstruction_supersession_binding).
narrative_ontology:cs_axiom_status(reconstruction_supersession_binding, holdable).
narrative_ontology:cs_axiom_grounding('74026de7-baff-4620-850d-4ecc15bf33b6', reconstruction_supersession_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('74026de7-baff-4620-850d-4ecc15bf33b6', amendment_cascade_authority).
narrative_ontology:cs_drift_state('74026de7-baff-4620-850d-4ecc15bf33b6', contemporary_originalist_challenge, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('74026de7-baff-4620-850d-4ecc15bf33b6', '').
narrative_ontology:cs_kernel_id(us_constitution__later_amendment_eras, us_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution__later_amendment_eras, freedmen_and_descendants).
narrative_ontology:constraint_beneficiary(us_constitution__later_amendment_eras, suffrage_expansion_coalitions).
narrative_ontology:constraint_beneficiary(us_constitution__later_amendment_eras, interpreters_wielding_amendment_authority).
narrative_ontology:constraint_victim(us_constitution__later_amendment_eras, defenders_of_original_settlement).
narrative_ontology:constraint_victim(us_constitution__later_amendment_eras, states_rights_traditionalists).
narrative_ontology:constraint_victim(us_constitution__later_amendment_eras, originalist_interpretive_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED UNDER ORIGINAL 1787 (SNARE) — Enslaved persons, women, non-property-holders were excluded from the founding text's political community. Their exit from this constraint required constitutional amendment—a mechanism not freely available to the powerless. The 13th, 14th, 15th amendments formally superseded the original settlement but only through forcible constitutional change imposed by the victors of the Civil War. Maximum extraction: the original frame denied personhood itself.
constraint_indexing:constraint_classification(us_constitution__later_amendment_eras, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FREEDMEN AND SUFFRAGE COALITIONS (TANGLED ROPE) — These groups benefited from the amendment process (13th, 14th, 15th, 19th, 26th) which formally expanded the political community and rights guarantees. But the amendments also locked them into a Constitution whose interpretive authority remained contested and subject to rollback through Jim Crow jurisprudence, Lochner-era retrenchment, and modern originalist reinterpretation. Genuine coordination benefit (legally enforceable rights) paired with asymmetric extraction (rights remain dependent on favorable judicial interpretation and sustained political organization).
constraint_indexing:constraint_classification(us_constitution__later_amendment_eras, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AMENDMENT RATIFYING COALITIONS (ROPE) — The political coalitions that successfully ratified the 13th, 14th, 15th, 19th, 26th amendments experienced the amendment process as pure coordination—solving the collective action problem of constitutional change. These coalitions benefited from the ratification (increased political base, legitimacy, enforceable commitments) and extracted no net cost. For them, the living amendment record is functional coordination.
constraint_indexing:constraint_classification(us_constitution__later_amendment_eras, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENDERS OF ORIGINAL SETTLEMENT (SNARE) — Parties invested in originalist or states-rights readings of the Constitution experience the later-amendment-eras reading as extractive suppression. Their interpretive framework—that the unamended 1787 text or the ante-Reconstruction constitutional order represents the authoritative baseline—is formally superseded by each wave of amendment. They face suppression: the amendment process precludes their exit from the expanded constitutional order (constitutional amendment requires supermajority, making reversal of rights-expansion amendments nearly impossible). The later-amendment reading naturalizes their loss as historical progress, rendering their framework illegitimate.
constraint_indexing:constraint_classification(us_constitution__later_amendment_eras, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL INTERPRETIVE AUTHORITY (TANGLED ROPE) — Courts wielding the living-amendment reading benefit from broad interpretive discretion: 'operative meaning set by successive amendment eras' permits reading contemporary values into the constitutional text without explicit formal amendment. This is both coordination (courts resolve ambiguities and apply constitutional principles to novel circumstances) and extraction (courts monopolize the authority to declare what the Constitution now means, bypassing amendment process for practical constitutional change). Moderate extractiveness because the judicial role is constrained by stare decisis, public reaction, and the amendment threat.
constraint_indexing:constraint_classification(us_constitution__later_amendment_eras, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective treating historical constitutional change as an inherent feature of legal systems, the living-amendment reading may appear as a natural law: constitutions must adapt or die; amendment cascades are inevitable as polities change; no written document can be truly unamended. However, this perspective risks naturalizing contingent political outcomes (the Civil War, Reconstruction's success, the 14th Amendment's eventual judicial vindication) as laws of constitutional physics. The engine's false-summit detector will flag this.
constraint_indexing:constraint_classification(us_constitution__later_amendment_eras, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution__later_amendment_eras_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution__later_amendment_eras, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution__later_amendment_eras, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution__later_amendment_eras, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution__later_amendment_eras_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The later-amendment-eras reading establishes that the Constitution's meaning is set by amendment coalitions, not fixed text. This creates asymmetric extraction: those able to form amendment coalitions can revise constitutional meaning; those defending prior settlements face suppression. The 13th, 14th, 15th amendments explicitly supersede prior constitutional provisions (slavery protections in Art. I, states' authority over suffrage in Art. II). Later amendments (19th, 26th) continue the pattern. For freedmen and suffrage-expansion groups, extractiveness is moderate because they benefit from the new frame even though their rights remain subject to judicial interpretation and political contestation. For defenders of the original settlement, extractiveness is high (they face suppression and have no exit from the amended order except constitutional convention, which is supermajority-required). Courts wielding the living-amendment reading extract moderate value—broad interpretive discretion paired with constraint from stare decisis and amendment threat. Suppression (0.58): Moderate-high. The later-amendment reading requires active suppression of originalist and states-rights counterreadings. This suppression was violent and legal-political in Reconstruction era (Civil War, military occupation, enforcement of 14th Amendment against Southern states), then became doctrinal (Plessy-era suppression of equal protection through judicial narrowing, later corrected by Warren Court). Contemporary suppression is institutional and professional (originalism is excluded from mainstream constitutional law scholarship and judicial interpretation, though revival is underway). Theater ratio (0.35): Moderate. The living-amendment reading is functional and formal: amendments ARE change to the constitutional text, not merely interpretation. But theater increases over time as the meaning of amendments becomes subject to judicial reinterpretation (Plessy v Ferguson misreads 14th Amendment equal protection; Warren Court and beyond correct this). Contemporary theater involves continuous performance of justifying why amendments override founding text against originalist challenge.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Freedmen and suffrage coalitions see the living-amendment reading as liberation (snare under 1787 becomes tangled_rope under 1868+). Amendment-ratifying coalitions see it as pure coordination (rope). Defenders of the original settlement see it as extractive suppression (snare). Courts see it as legitimate authority-creation (tangled_rope, benefiting from interpretive discretion). Originalists see it as illegitimate rewriting (they see rope or scaffold at best for the amendment process itself, but mountain/immutable for the founding text). The analytical observer risks seeing it as natural law (amendments naturally supersede old law) but the constraint's structural data (beneficiaries, suppression requirement, extractiveness renegotiated era by era) reveals this as a false summit: the 'naturalness' of amendment supersession is a politically contingent outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from the agent's relationship to the extraction flow. Freedmen/suffrage coalitions are victims of the original 1787 frame but beneficiaries of the living-amendment frame—their directionality d shifts dramatically (1787: d ≈ 1.0 = full target; 1868+: d ≈ 0.6 = mixed victim-beneficiary). Defenders of the original settlement shift the opposite direction (1787: d ≈ 0.0 = full beneficiary; 1868+: d ≈ 0.8 = primary victim). Amendment-ratifying coalitions (d ≈ 0.3 = net beneficiary from ratification authority) gain power through the process. Judicial authority (d ≈ 0.4 = moderate beneficiary from interpretive discretion) extracts value through interpretation. The perspectives' classifications reflect these d values: high d produces snare (powerless with high d, or moderate/constrained with d ≈ 0.8); moderate d produces tangled_rope; low d produces rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the constraint's readings according to their authority structures. The later-amendment-eras reading declares that amendment processes set operative meaning. This is NOT an empirical claim about which text is 'truer' or 'more original,' but a NORMATIVE claim about where constitutional authority RESIDES. Other readings (original_constitution_1787, bill_of_rights_1791, pre_constitutional_frameworks) declare authority resides elsewhere. The classifications diverge not because the readings measure different things, but because they disagree about the legitimate locus of constitutional authority. This is a conceptual rather than empirical mandatrophy. The later-amendment-eras reading reduces mandatrophy within its own frame by clearly locating authority in the amendment cascade and treating successive amendments as the constitutional development record. But across readings, mandatrophy persists: which authority structure is legitimate? This is routed to omega variables (reading_succession_legitimacy, interpretation_authority_locus) rather than forcing a single classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_succession_mechanism,
    'Does the living-amendment reading describe a structural mechanism of constitutional authority, or does it naturalize the contingent political victory of amendment coalitions?',
    'Comparative analysis: do other constitutional orders (Canada, Australia, Weimar, post-1949 Germany) show the same ''amendment cascades override founding text'' pattern, or is US amendment history path-dependent on Civil War and Reconstruction?',
    'If structural universal: living-amendment reading describes a law of constitutional politics (closer to mountain classification). If path-dependent: the reading naturalizes political victories, warranting false-summit status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_succession_mechanism, conceptual, 'Whether amendment succession is a structural law or contingent historical path').

omega_variable(
    reconstruction_ascendancy_legitimacy,
    'What grounds the claim that Reconstruction amendments supersede the founding settlement? Military victory, popular ratification, moral progress, or formal amendment procedure?',
    'Jurisprudential analysis of reconstruction-era legitimacy doctrines; comparison with contemporary-era constitutional interpretation (is later amendment automatically superior, or do specific amendments claim different authority?)',
    'If based on military victory alone: the reading is about power, not law (reclassifies extractiveness upward). If based on ratification and moral progress: coordination with asymmetric extraction (tangled_rope confirmed). If based on formal procedure: pure coordination (rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reconstruction_ascendancy_legitimacy, conceptual, 'Legitimacy grounds for Reconstruction amendment supremacy').

omega_variable(
    original_settlement_suppression_mechanism,
    'Is the original 1787 settlement legally suppressed (overruled) by later amendments, or merely reinterpreted through the lens of later amendments?',
    'Constitutional text analysis: do later amendments repeal specific 1787 provisions explicitly (13th abolishes slavery per Art. I Sec. 2 implicit protection; 19th repudiates sex-based voting restrictions), or do courts interpolate modern meaning into ambiguous original provisions?',
    'If explicit repeal: clear succession mechanism, victims are real (originalists defending overruled provisions). If interpolation: the suppression is judicial and contested, opening space for originalist counterargument that the 1787 text remains supreme law unless amended.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_settlement_suppression_mechanism, empirical, 'Whether later amendments explicitly repeal or reinterpret original provisions').

omega_variable(
    interpretive_authority_locus,
    'Who sets the operative meaning according to the living-amendment reading: the amendment text itself, the amendment ratification coalition''s intent, contemporary courts, or the current ratifying supermajority?',
    'Doctrinal analysis of living constitutionalism schools (evolutionary interpretation, moral reading, democratic interpretation); case law showing where courts ground meaning claims',
    'If amendment text alone: pure formal constraint (rope). If ratification intent: historical constraint on interpretation (tangled_rope). If contemporary courts: judicial extraction (snare or tangled_rope depending on constraints). If ratifying supermajority: pure coordination (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_locus, conceptual, 'Locus of interpretive authority in living-amendment reading').

omega_variable(
    kernel_contested_reading_ambiguity,
    'This constraint is ONE reading of the US Constitution kernel. The sibling readings (bill_of_rights_1791, original_constitution_1787, pre_constitutional_frameworks, failed_amendments) each locate constitutional authority differently. Can all five readings coexist in contemporary jurisprudence, or does adoption of the later-amendment-eras reading foreclose the others?',
    'Jurisprudential inventory: survey contemporary constitutional discourse and case law to determine which readings are held by live coalitions of interpreters. Foreclosure analysis: does accepting that amendments set operative meaning logically prevent acceptance that the 1787 text is supreme (original_constitution_1787) or that failed amendments define the Constitution (failed_amendments)?',
    'If coexistence: all readings are live positions (reading_relations use coexists_with). If the later-amendment reading forecloses original_constitution_1787: the two cannot coexist in one framework (reading_relations use forecloses). If the later-amendment reading influences but does not foreclose: structural pressure without logical contradiction (reading_relations use influences).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contested_reading_ambiguity, conceptual, 'Logical and empirical coexistence of constitutional readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution__later_amendment_eras, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_amend_tr_t0, us_constitution__later_amendment_eras, theater_ratio, 0, 0.2).
narrative_ontology:measurement(const_amend_tr_t3, us_constitution__later_amendment_eras, theater_ratio, 3, 0.35).
narrative_ontology:measurement(const_amend_tr_t6, us_constitution__later_amendment_eras, theater_ratio, 6, 0.35).
narrative_ontology:measurement(const_amend_tr_t9, us_constitution__later_amendment_eras, theater_ratio, 9, 0.35).

% Extraction over time
narrative_ontology:measurement(const_amend_be_t0, us_constitution__later_amendment_eras, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(const_amend_be_t3, us_constitution__later_amendment_eras, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(const_amend_be_t6, us_constitution__later_amendment_eras, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(const_amend_be_t9, us_constitution__later_amendment_eras, base_extractiveness, 9, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(const_amend_su_t0, us_constitution__later_amendment_eras, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(const_amend_su_t3, us_constitution__later_amendment_eras, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(const_amend_su_t6, us_constitution__later_amendment_eras, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(const_amend_su_t9, us_constitution__later_amendment_eras, suppression_requirement, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution__later_amendment_eras, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution__later_amendment_eras, us_constitution__original_constitution_1787).
narrative_ontology:affects_constraint(us_constitution__later_amendment_eras, us_constitution__bill_of_rights_1791).
narrative_ontology:affects_constraint(us_constitution__later_amendment_eras, us_constitution__pre_constitutional_frameworks).
narrative_ontology:affects_constraint(us_constitution__later_amendment_eras, us_constitution__failed_amendments).

% DUAL FORMULATION NOTE:
% The US Constitution kernel is decomposed into five constraint stories, each representing a reading that locates constitutional authority differently. The later-amendment-eras reading (this constraint) declares that amendments set operative meaning and treats the founding text as a baseline to be superseded. Each sibling reading (original_constitution_1787, bill_of_rights_1791, pre_constitutional_frameworks, failed_amendments) produces a different extractiveness value and different beneficiary/victim structures because each claims a different authority locus. Network links show structural influence: the later-amendment-eras reading influences all siblings by subordinating them to the amendment cascade. The original_constitution_1787 reading forecloses or coexists_with the later-amendment reading depending on whether 1787-text supremacy can coexist with amendment-authority supremacy in contemporary jurisprudence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution__later_amendment_eras, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
