% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence: Thinkability Reading (1710)
 *   domain: legal_philosophy/intellectual_property/jurisprudence
 *
 * SUMMARY:
 *   The thinkability reading instantiates one axis of a contested kernel
 *   about IP emergence in 1710. This reading emphasizes that before the
 *   Statute of Anne, expression could not be conceived as ownable property in
 *   a unified, coherent way; after 1710, 'copy right' emerged as a distinct
 *   legal category, separable from guild privilege and property-in-labor. The
 *   reading's claim is that the category itself—the thinkability of 'ownable
 *   expression'—was the transformative achievement. This reading coexists
 *   with alternatives: first_holding_reading (which emphasizes the emergence
 *   of authors as legitimate rights-holders rather than category coherence)
 *   and synchronic_diachronic_seam (which questions whether thinkability and
 *   first-holding are formally independent or merely temporal framings of the
 *   same event). This story instantiates ONLY the thinkability reading;
 *   sibling stories express the other readings.
 *
 * KEY AGENTS:
 *   - author_claimants: independent authors and printers who benefit from a coherent 'copy right' category and can now defend their claims without guild membership
 *   - guild_monopoly_defenders: Stationers' Company and privilege-holders who lose certainty when claims can be framed as property rather than privilege
 *   - jurists_and_commentators: judges, legal theorists, and parliamentary draftsmen who set the agenda by codifying 'copy right' as a category
 *   - legal_system_coherence: the abstract good of doctrinal clarity and predictability in adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.42).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.28).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence: Thinkability Reading (1710)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/jurisprudence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '75a616f3-eead-41d2-8c78-243962aae2ed').
narrative_ontology:cs_kernel_codification('75a616f3-eead-41d2-8c78-243962aae2ed', formalized).
narrative_ontology:cs_authority_grounding('75a616f3-eead-41d2-8c78-243962aae2ed', lineage).
narrative_ontology:cs_interpretation_layer_present('75a616f3-eead-41d2-8c78-243962aae2ed').
narrative_ontology:cs_reading_relation('75a616f3-eead-41d2-8c78-243962aae2ed', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('75a616f3-eead-41d2-8c78-243962aae2ed', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('75a616f3-eead-41d2-8c78-243962aae2ed', foundational, expression_thinkability_prerequisite).
narrative_ontology:cs_axiom_status(expression_thinkability_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('75a616f3-eead-41d2-8c78-243962aae2ed', expression_thinkability_prerequisite, instrumental).
narrative_ontology:cs_axiom('75a616f3-eead-41d2-8c78-243962aae2ed', secondary, category_coherence_enables_claims).
narrative_ontology:cs_axiom_status(category_coherence_enables_claims, holdable).
narrative_ontology:cs_axiom_grounding('75a616f3-eead-41d2-8c78-243962aae2ed', category_coherence_enables_claims, instrumental).
narrative_ontology:cs_reference_frame('75a616f3-eead-41d2-8c78-243962aae2ed', pre_statutory_privilege_monopoly).
narrative_ontology:cs_drift_state('75a616f3-eead-41d2-8c78-243962aae2ed', post_statute_of_anne_codification, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('75a616f3-eead-41d2-8c78-243962aae2ed', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, author_claimants).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, legal_system_coherence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, guild_monopoly_defenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors, printers, and booksellers who benefit from the emerging category of 'copy right' as a distinct, alienable property right separable from guild monopoly. The thinkability of ownable expression gives them legal language to defend their claims without relying on guild membership or Crown letters patent. They collect the benefit of category coherence: their disputes now map to a legible framework rather than ad-hoc privilege claims.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, author_claimants, beneficiary,
    moderate, generational, constrained, national).

% The abstract good of doctrinal coherence: the legal system gains a named category ('copy right') that disambiguates disputes that previously lacked vocabulary. This is a vindicated proposition, not a real actor, but it collects the benefit of reduced confusion and improved predictability in adjudication.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_system_coherence, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(ip_category_emergence__thinkability_reading, legal_system_coherence).

% Established guild structures (Stationers' Company) that relied on Crown-granted monopolies face pressure as 'copy right' offers an alternative legitimacy frame. Their monopoly rents are now contestable by those outside the guild who can argue for property rights rather than begging privilege. They pay in reduced certainty of their monopoly position and must now defend their claims in property language rather than privilege language.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, guild_monopoly_defenders, payer,
    organized, biographical, constrained, national).

% Printers, authors, and booksellers operating before 1710 who could not deploy IP language because it did not yet exist as a coherent category. Their disputes were adjudicated under privilege, guild-right, or property-in-labor frames, none of which were stable or universal. They are excluded by the temporal boundary: they could not have used the category when it was not thinkable.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, pre_1710_dispute_participants, excluded,
    organized, biographical, trapped, national).

% Judges, legal theorists, and parliamentary draftsmen who codify and interpret the emerging 'copy right' category. They set the agenda by choosing how to name the right, what it attaches to, and how it relates to other property forms. Their decisions embed the thinkability into doctrine; their rulings make the category real and actionable.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, jurists_and_commentators, agenda_setter,
    institutional, generational, analytical, national).

% Readers and consumers of printed works who do not participate in authorship or publishing decisions. The emergence of IP category affects them indirectly through pricing, access, and what gets printed, but they are not seats in the category-emergence constraint itself—they are outside looking in.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, public_as_readers, observer,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__thinkability_reading, legal_system_coherence).
narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The legal system gains a coherent conceptual frame for disputes over ownable expression: instead of ad-hoc appeals to privilege, guild-right, or labor-property, disputes can now be articulated in the language of 'copy right' as a distinct, transferable right. This coordination function reduces confusion in adjudication and allows strangers (people outside guild structures) to invoke the same frame.
% TRANSFER_FUNCTION: Authority to name and defend claims to printed expression transfers from guild monopoly holders and Crown privilege-grantees to a broader class of claimants (independent authors, outsider printers, booksellers without guild membership). The thinkability of the category as property (rather than privilege) redistributes who can legitimately claim authority over a work.
% ABSENT_VOICES: Pre-1710 participants in manuscript and printing disputes would object if resurrected: they lacked the vocabulary to reframe their conflicts in IP terms, and the retroactive application of 'copy right' language to their disputes naturalizes a category that was not available to them. Authors working in non-English jurisdictions or outside the book trades might also object: the emergence of IP as a category happens within a specific legal system and commercial context; they experience it as jurisdiction-specific or irrelevant.
% DISAPPEARANCE_RATIONALE: If the thinkability of 'copy right' as a distinct category disappeared—if the legal vocabulary collapsed and disputes reverted to formulations like 'guild privilege' or 'property in labor'—the landscape of adjudication would rearrange: claimants would lose the stable frame they had gained, disputes would proliferate in incoherent forms, and the legal system would lose the doctrinal coherence it had achieved. However, the underlying disputes (who controls printing, who benefits from authorship) would not disappear; they would simply be fought in non-IP language. The verdict is contested because some parties (guild defenders, privilege-holders) would argue that the world would be stable without the category—that IP merely renamed pre-existing arrangements—while others (independent claimants, jurists) would argue that the category was transformative and its absence would derange the entire system.
% FOUNDING_PROBLEM: Before 1710, disputes over control and benefit from printed expression lacked a stable legal vocabulary. Claimants invoked guild membership, Crown letters patent, property-in-labor, and breach of confidence—ad-hoc frames that did not cohere across cases. The legal system had no unified way to think about whether expression could be owned, by whom, for how long, or under what conditions.
% FOUNDING_PROBLEM_CORROBORATION: Literary historians and legal scholars (including statute commentators from outside the benefiting parties—judges and academics not employed by publishers) document that pre-1710 disputes were indeed incoherent in framing: the same dispute could be articulated as breach of privilege, property in labor, copyright, or mere contract, depending on the court and the claimant's relationship to guild structures. Post-1710 commentaries (Blackstone, later IP theorists) confirm that the emergence of 'copy right' as a distinct category was an interpretive achievement, not a discovery of pre-existing natural law. The Statute of Anne (1710) itself is the pivot-point corroboration: it creates the legal vocabulary.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, contested).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).
:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the category emergence is a genuine coordination achievement—it reduces confusion and gives claimants a stable frame—but it is also embedded in property extraction; the thinkability of ownable expression enables rent-collection and gate-keeping. The measurement series shows extractiveness rising from 0.18 (pre-1710, disputes incoherent, no category yet) to 0.42 (post-1710, category stabilized, but extraction now normalized), then plateauing. Suppression is low and declining (0.28 at t=40, down from 0.35 at t=0) because the category gain reduces the need for active suppression—the frame itself does the work. Theater is consistently low (~0.15) because the category-emergence is substantive (disputes do map better to the new frame) rather than performative. Accessibility-collapse is moderately high (0.68) because once 'copy right' becomes thinkable, pre-1710 alternatives (privilege, labor-property) become harder to access; the new category crowds out older frames. Resistance is low-moderate (0.35) because the category addresses a real coordination problem that guild-monopoly defenders can also use; it is not purely extractive, so opposition is muted.
 *
 * PERSPECTIVAL GAP:
 *   The jurist/agenda-setter seat experiences this as the achievement of doctrinal clarity and legal progress—a genuinely beneficial category emergence. The guild-defender seat experiences it as loss of monopoly certainty and pressure to defend claims in a new frame that does not favor them. The author-claimant seat experiences it as liberation—they can now invoke property language instead of begging Crown favor. The pre-1710 excluded seat (historically, no voice now) would experience it as retroactive rewriting of their disputes in language they never had. The engine computes these perspectival divergences from power + exit + role; the measurement series documents the shared temporal arc of increasing extractiveness even as the story is claimed as rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Author-claimants are beneficiaries: they gain access to a stable frame (d near 0.2). Guild-defenders are payers: they lose monopoly certainty (d near 0.8). Jurists are the agenda-setter (d near 0.5, they set the frame but do not profit from it directly). Legal-system-coherence is not an agent and collects no extraction (agent: false). The directionality mapping depends on the reading: this reading emphasizes the thinkability gain, which is a genuine coordination benefit, so beneficiaries and payers are distributed by who gains clearer frames vs. who loses monopoly power, not by who extracts rents. This differs from the first_holding_reading, which would emphasize authors as NEW legitimate claimants (different beneficiary structure) and measure different d values for the same agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (disputes over expression control lack coherent vocabulary) was live and urgent before 1710. The Statute of Anne solved it by codifying 'copy right' as a distinct category. Does the problem stay live post-1710? Jurists and legislators say yes—disputes still arise about scope, duration, and who qualifies. Guild-defenders and some privilege-holders might say the problem was solved by privilege itself (no category needed). This tension is captured in the founding_problem_status = 'live' and the contested disappearance_verdict: some parties argue the category was essential to solving coordination; others argue privilege did the job and IP merely renamed it. The measuring body tracks this by the M4/M5 seam (synchronic_diachronic_seam reading): if thinkability and first-holding are formally independent, then founding_problem resolution tracks thinkability alone; if they are temporal reframing of the same event, then the constraint's mandate is fused with the first-holding reading and cannot be separated here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_vs_discovery,
    'Was ''copy right'' as a distinct category a genuine emergence (previously unthinkable, became thinkable in 1710), or was it a discovery of something that already existed in practice?',
    'Textual and doctrinal history: examine pre-1710 disputes for evidence of whether parties used or could have used IP-like vocabulary, or whether they were forced into privilege/guild frames by linguistic necessity. If pre-1710 texts show authors invoking property-in-expression language successfully (even if not named ''copy right''), the emergence was discovery, not creation.',
    'If genuinely emergent, the constraint is a coordinate achievement (rope-type); the category change is real. If discovered, the constraint might be reclassified as piton (the category existed but became thinkable/codified). High impact on type computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_vs_discovery, conceptual, 'Whether the 1710 category was a genuine linguistic/conceptual creation or a pre-existing practice made legible.').

omega_variable(
    category_independence_from_first_holding,
    'Are thinkability (coherent concept) and first-holding (authors as legitimate claimants) formally independent events, or are they temporal framings of a single structural emergence?',
    'Formal analysis per OQ-254 (M4/M5 seam test): if you can construct a coherent history where (a) authors became thinkable as rights-holders WITHOUT the category being thinkable (or vice versa), they are independent; if you cannot separate them without contradiction, they are fused framings of one event. Witness: can you imagine a world where guild-monopoly persists but authors gain property rights through a different category (labor-property, trust, equity)? If yes, independent; if no, fused.',
    'If independent, this constraint stands alone and sibling readings are genuinely distinct constraints. If fused, the thinkability_reading and first_holding_reading refer to the same underlying emergence and should be collapsed into one story with multiple framings (not done here, per Rule 1).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_independence_from_first_holding, conceptual, 'Formal separability of thinkability and first-holding as independent events or fused moments.').

omega_variable(
    suppression_internalization_trajectory,
    'Is the measured suppression decline (0.35 → 0.28 over 40 years) because the category-frame itself does the work (internalized acceptance), or because external enforcement fades and the category becomes performative?',
    'Post-emergence dispute data: if suppression declines AND dispute outcomes converge (fewer appeals, faster adjudication, party acceptance), the frame is doing the work and suppression is internalized. If suppression declines but dispute outcomes diverge (parties contest category application, invent workarounds, appeal more), the frame is performative and suppression is just fading.',
    'If internalized, the category is genuinely accepted and the constraint is stable rope. If performative, the constraint is piton-trajectory (function atrophies, theatrical maintenance increases). Theater-ratio stays low (0.15) either way, so the mechanism matters for stability prognosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Whether declining suppression indicates internalized frame acceptance or fading performative enforcement.').

omega_variable(
    kernel_reading_framing_choice,
    'Is this reading (thinkability) the defensible primary framing of 1710 IP emergence, or is one of the sibling readings more fundamental?',
    'Jurisprudential genealogy: trace which framing the 18th and 19th-century commentators (Blackstone, Justice Mansfield, later theorists) actually emphasized. If they emphasize author-as-claimant, first_holding is primary. If they emphasize the category''s coherence, thinkability is primary. If they treat both symmetrically or collapse them, the seam reading is empirically grounded.',
    'If thinkability is the defensible primary framing, this constraint grounds the others; if not, this reading is an optional lens and the primary emerges from canonical sources. No impact on type computation (the engine decides based on metrics), but high impact on which story is pedagogically foundational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, empirical, 'Whether thinkability is the primary or secondary framing of 1710 IP emergence in legal tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__thinkability_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ip_c_tr_t5, ip_category_emergence__thinkability_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(ip_c_tr_t10, ip_category_emergence__thinkability_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(ip_c_tr_t15, ip_category_emergence__thinkability_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(ip_c_tr_t25, ip_category_emergence__thinkability_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(ip_c_tr_t40, ip_category_emergence__thinkability_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__thinkability_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ip_c_be_t5, ip_category_emergence__thinkability_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(ip_c_be_t10, ip_category_emergence__thinkability_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(ip_c_be_t15, ip_category_emergence__thinkability_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(ip_c_be_t25, ip_category_emergence__thinkability_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(ip_c_be_t40, ip_category_emergence__thinkability_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__thinkability_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ip_c_su_t5, ip_category_emergence__thinkability_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(ip_c_su_t10, ip_category_emergence__thinkability_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(ip_c_su_t15, ip_category_emergence__thinkability_reading, suppression_requirement, 15, 0.29).
narrative_ontology:measurement(ip_c_su_t25, ip_category_emergence__thinkability_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement(ip_c_su_t40, ip_category_emergence__thinkability_reading, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__thinkability_reading, 0.05).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% Part of ip_category_emergence constraint family. The kernel is the 1710 Statute of Anne and the emergence of IP as a legal category. Three readings decompose on the question of what 'emergence' means: (1) thinkability_reading: coherence of 'copy right' as a distinct category (this story); (2) first_holding_reading: emergence of authors as a new legitimate claimant class; (3) synchronic_diachronic_seam: formal independence or fusion of (1) and (2). Each reading is a separate constraint story with its own ε, beneficiary/victim structure, and metrics. The seam reading influences both thinkability and first_holding because its resolution affects whether they can be measured as distinct constraints or must be fused.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__thinkability_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
