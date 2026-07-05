% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__reformist_ijtihad, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad: Contextual Reinterpretation Mandate over Classical Rulings
 *   domain: religious/legal/social
 *
 * SUMMARY:
 *   This story instantiates the reformist_ijtihad reading of the
 *   quran_hadith_substrate kernel: the position that contextual ijtihad is
 *   mandated whenever classical rulings conflict with contemporary ethics,
 *   human rights norms, or maslaha, and that the Quran's ethical trajectory
 *   should be prioritized over literalist hadith application. This is one of
 *   three structurally distinct readings of the same underlying kernel (the
 *   traditionalist_taqlid reading and the state_hybrid reading are separate
 *   constraint stories, linked here via network.affects_constraints). The
 *   reformist reading is authored as a tangled_rope: it performs genuine
 *   coordination work (letting adaptation happen without requiring exit from
 *   the tradition) while also displacing an existing authority structure
 *   whose legitimacy depended on interpretive finality — that displacement is
 *   a real cost borne by traditional and Salafi authorities, not merely a
 *   disagreement about doctrine.
 *
 * KEY AGENTS:
 *   - progressive_muslim_scholars: agenda_setter (organized/constrained) — administers the reinterpretive method
 *   - muslim_women_seeking_reform: beneficiary (moderate/constrained) — gains doctrinal leverage for reform
 *   - lgbtq_muslims: beneficiary (powerless/trapped) — most dependent on the reading, least able to advocate for it
 *   - traditional_madhhab_authorities: payer (institutional/identity_locked) — loses interpretive monopoly
 *   - salafi_and_traditionalist_clerics: payer/excluded (organized/identity_locked) — treats the method itself as illegitimate
 *   - state_religious_authorities: observer/beneficiary (institutional/analytical) — uses the reading instrumentally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.32).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.28).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.32).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad: Contextual Reinterpretation Mandate over Classical Rulings").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal/social").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '2a73b284-cec8-451d-98d9-4569c1110d2b').
narrative_ontology:cs_kernel_codification('2a73b284-cec8-451d-98d9-4569c1110d2b', distributed).
narrative_ontology:cs_authority_grounding('2a73b284-cec8-451d-98d9-4569c1110d2b', expertise).
narrative_ontology:cs_interpretation_layer_present('2a73b284-cec8-451d-98d9-4569c1110d2b').
narrative_ontology:cs_reading_relation('2a73b284-cec8-451d-98d9-4569c1110d2b', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('2a73b284-cec8-451d-98d9-4569c1110d2b', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('2a73b284-cec8-451d-98d9-4569c1110d2b', foundational, quranic_ethical_trajectory_supersedes_literalist_hadith).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_supersedes_literalist_hadith, holdable).
narrative_ontology:cs_axiom_grounding('2a73b284-cec8-451d-98d9-4569c1110d2b', quranic_ethical_trajectory_supersedes_literalist_hadith, conventional).
narrative_ontology:cs_axiom('2a73b284-cec8-451d-98d9-4569c1110d2b', foundational, maslaha_and_contemporary_human_rights_norms_are_valid_interpretive_criteria).
narrative_ontology:cs_axiom_status(maslaha_and_contemporary_human_rights_norms_are_valid_interpretive_criteria, holdable).
narrative_ontology:cs_axiom_grounding('2a73b284-cec8-451d-98d9-4569c1110d2b', maslaha_and_contemporary_human_rights_norms_are_valid_interpretive_criteria, instrumental).
narrative_ontology:cs_axiom('2a73b284-cec8-451d-98d9-4569c1110d2b', secondary, ijma_of_classical_schools_is_revisable_not_final).
narrative_ontology:cs_axiom_status(ijma_of_classical_schools_is_revisable_not_final, holdable).
narrative_ontology:cs_axiom_grounding('2a73b284-cec8-451d-98d9-4569c1110d2b', ijma_of_classical_schools_is_revisable_not_final, conventional).
narrative_ontology:cs_reference_frame('2a73b284-cec8-451d-98d9-4569c1110d2b', classical_ijma_interpretive_finality).
narrative_ontology:cs_drift_state('2a73b284-cec8-451d-98d9-4569c1110d2b', post_human_rights_era_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2a73b284-cec8-451d-98d9-4569c1110d2b', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, muslim_women_seeking_reform).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities_in_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, reformist_institutions_and_ngos).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_madhhab_authorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, salafi_and_traditionalist_clerics).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, rural_conservative_congregations_facing_doctrinal_disruption).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, state_religious_authorities).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, quranic_ethical_trajectory_thesis).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, maslaha_as_legitimate_interpretive_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and circulate reinterpretive fiqh that subordinates literalist hadith application to Quranic ethical trajectory and maslaha analysis. They administer the interpretive method itself — deciding which classical rulings are contextually superseded — and depend on institutional platforms (universities, reform councils, transnational networks) to have that method taken as authoritative rather than heretical.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_scholars, agenda_setter,
    organized, generational, constrained, global).

% Gain doctrinal grounds to contest classical rulings on guardianship, inheritance, marriage, and testimony that disadvantage them, using reformist ijtihad as religiously legitimate cover for demands they could not otherwise press within a taqlid framework. Their exit from the religious community entirely remains costly, so the reformist reading matters precisely because it lets them stay inside the tradition while changing its application.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, muslim_women_seeking_reform, beneficiary,
    moderate, biographical, constrained, national).

% Depend most heavily on the reformist reading's willingness to treat classical criminalizing rulings as historically contingent rather than eternally binding; without institutional backing for this reading in their specific community, the benefit is theoretical rather than lived. Exit from family and community networks is often not a real option, which is why the doctrinal argument matters more to this group than to any other beneficiary.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims, beneficiary,
    powerless, biographical, trapped, global).

% Benefit indirectly when reformist ijtihad displaces classical dhimmi-status rulings with human-rights-framed equal citizenship arguments. Have essentially no capacity to advocate for this reading themselves — they are recipients of a fight being waged inside the Muslim community, not participants in it.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities_in_muslim_majority_states, beneficiary,
    powerless, generational, trapped, national).

% Their institutional legitimacy rests on the claim that ijma and established madhhab consensus already settled these questions and that taqlid is obligatory. Reformist ijtihad directly displaces their interpretive monopoly by arguing that any ruling can be revisited against contemporary ethics — this is an identity-constitutive threat, not merely a doctrinal disagreement, because their authority is inseparable from the claim that the classical corpus does not need re-litigating.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_madhhab_authorities, payer,
    institutional, civilizational, identity_locked, global).

% Regard the prioritization of ethical trajectory over hadith application as an illegitimate innovation (bid'ah) that severs the chain of textual authority. They lose the ability to declare a ruling closed and settled once reformist method is granted standing, and they are largely excluded from the reformist institutions where the counter-argument would need to be made on equal footing.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, salafi_and_traditionalist_clerics, payer,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, salafi_and_traditionalist_clerics, excluded).

% Experience reformist ijtihad as destabilizing when it arrives via state policy, satellite media, or NGO programming without local buy-in — long-settled communal practices (inheritance division, marriage procedure) are challenged from outside, and the community bears the social cost of contested legitimacy without having chosen the change.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, rural_conservative_congregations_facing_doctrinal_disruption, payer,
    powerless, biographical, constrained, local).

% Selectively invoke reformist ijtihad to modernize family or criminal codes when politically convenient, without committing to the reformist method as a general principle. Their interest in this reading is instrumental — they observe which arguments produce usable legal cover — which distinguishes them from committed reformist scholars even where their public positions overlap.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, state_religious_authorities, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, state_religious_authorities, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__reformist_ijtihad, diffuse).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__reformist_ijtihad, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a religiously legitimate mechanism for Muslim communities to revise rulings that have become ethically untenable or practically unworkable, without requiring wholesale exit from the tradition — it coordinates continuity of religious identity with adaptation to changed circumstances.
% TRANSFER_FUNCTION: Moves interpretive authority away from traditional madhhab custodians and toward scholars, jurists, and institutions willing to apply contextual method against classical precedent; moves practical legal and social standing toward groups (women, sexual minorities, religious minorities) previously disadvantaged by literalist application.
% ABSENT_VOICES: Ordinary congregants in conservative and rural communities are rarely present in the scholarly and NGO-mediated spaces where reformist ijtihad is debated; they experience its effects (changed marriage procedures, contested inheritance norms) as decisions made elsewhere and imposed locally.
% DISAPPEARANCE_RATIONALE: If contextual ijtihad as a legitimating method vanished, progressive scholars would lose their primary doctrinal tool, women's and minority rights advocacy grounded in religious argument would have to shift entirely to secular or human-rights framing, and traditional authorities would regain uncontested claim to interpretive finality — family law reform movements across multiple states would lose their religious cover and likely stall or shift register entirely.
% FOUNDING_PROBLEM: Classical fiqh, formed in and for premodern social conditions, produces rulings (on slavery, women's testimony and guardianship, apostasy, minority status) that a large and growing population of Muslims experiences as ethically indefensible or practically unworkable against contemporary human rights norms and lived circumstances, while still wanting to remain within an Islamic ethical framework rather than exit it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the reformist camp itself by empirical evidence of forum-shopping and quiet accommodation within traditionalist institutions (fatwa councils issuing exceptions under maslaha language without naming reformist method), by comparative legal scholarship documenting divergence between codified family law and classical fiqh across multiple Muslim-majority states, and by traditionalist critics who, in the course of attacking reformist ijtihad, concede that the underlying tensions it responds to are real even while rejecting the proposed remedy.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).
:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.32) and rising slowly, reflecting the ε bin specified for this reading: real but not severe, since the reading's institutional backing varies enormously by country and community and the reformist camp does not hold coercive enforcement power comparable to state or classical clerical bodies. Suppression is authored moderate-low and declining (0.40 to 0.28) because as reformist institutions gain footholds (universities, transnational fiqh councils, some state backing) the need to actively suppress traditionalist counter-argument decreases — the reading survives more by persuasion and institutional accretion than by coercion, which is the structural opposite of the traditionalist reading's likely profile. Resistance is authored high (0.72) because traditionalist and Salafi authorities mount sustained, organized doctrinal and political opposition; this is a genuinely contested reading, not a settled one. Accessibility collapse is authored low (0.30): the traditionalist alternative remains fully available and, in most Muslim-majority contexts, dominant — reformist ijtihad has not closed off the classical alternative, it competes with it.
 *
 * PERSPECTIVAL GAP:
 *   Progressive scholars and beneficiary groups experience this constraint as liberatory coordination — a mechanism that lets them remain within an Islamic ethical framework while securing substantive change. Traditional and Salafi authorities experience the identical structure as extractive delegitimation — their claim to interpretive finality is the very thing being taken from them. The engine should compute these as different seat classifications from the same structural facts; that divergence is expected and is exactly the point of a tangled_rope authored at moderate ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (progressive scholars, women, LGBTQ+ Muslims, religious minorities) are declared with low-to-moderate power and constrained-to-trapped exit, which the engine should read as directionality near the beneficiary end for what the constraint does for them, even though several of these groups hold little power generally — the constraint is one of the few levers available to them precisely because their general power is low. Victims (traditional madhhab authorities, Salafi clerics) hold institutional/organized power but are declared identity_locked on exit, because their objection to reformist ijtihad is not a resource dispute but an identity-constitutive one: their authority IS the claim that interpretation is settled. State religious authorities are treated as an instrumental observer/beneficiary rather than a committed party, since their backing of reformist arguments tracks political convenience rather than doctrinal conviction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (classical fiqh producing outcomes many contemporary Muslims find ethically indefensible) is authored as live, not dead — this blocks a mandatrophy misreading in either direction. It prevents mislabeling reformist ijtihad as pure extraction (the underlying ethical tension is real and independently corroborated, not manufactured to justify the reformist project) and it also prevents treating the reading as costless pure coordination (identifiable parties — traditional authorities — do lose real legitimacy and standing through its operation, which is why requires_active_enforcement and a victim list are both authored).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformist_method_as_genuine_or_convenient,
    'Is contextual ijtihad, as practiced by reformist scholars, a methodologically disciplined application of classical usul al-fiqh principles (maslaha, maqasid) extended to new contexts, or is it a results-oriented method that selects whichever classical tool yields a predetermined progressive conclusion?',
    'Comparative analysis of reformist scholars'' rulings across cases where maslaha-based reasoning would predict outcomes AGAINST the progressive position, checking for methodological consistency versus outcome-steering.',
    'If methodologically consistent, the reformist reading has a stronger claim to intra-traditional legitimacy and lower effective extraction; if outcome-steering, it is more vulnerable to the traditionalist charge of being a secular agenda dressed in fiqh language, which would raise its effective suppression cost over time as the charge gains traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_method_as_genuine_or_convenient, conceptual, 'Whether reformist ijtihad''s method is principled or results-driven.').

omega_variable(
    institutional_backing_variance,
    'How much does the ε value for this reading vary across jurisdictions with strong reformist institutional backing (e.g., Tunisia, Malaysia''s Sisters in Islam contexts) versus jurisdictions where reformist scholars operate under threat or exile?',
    'Country-by-country tracking of institutional backing (state endorsement, university chairs, fatwa council representation) against measured social and legal outcomes for the beneficiary groups.',
    'The story authors a single moderate ε (0.32) as a cross-context central estimate per the ε-invariance principle, but the true variance across contexts may be wide enough that a future decomposition into jurisdiction-specific stories would be warranted if local ε values diverge sharply from this estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_backing_variance, empirical, 'Cross-jurisdictional variance in the reading''s institutional strength and effective extraction.').

omega_variable(
    framing_underdetermination_kernel_vs_authority_claim,
    'Is the correct unit of analysis the interpretive METHOD (contextual ijtihad as a hermeneutic technique) or the LEGITIMACY CLAIM layered above it (the assertion that this method''s outputs deserve equal or superior standing to classical consensus)? These two framings could yield different cs_pattern classifications: the method alone looks like a scholarly practice (closer to expertise-grounded authority), while the legitimacy claim looks like a direct challenge to an existing authority structure (closer to a contested-extraction pattern).',
    'Track whether reformist arguments are evaluated by opponents primarily on methodological grounds (is this valid ijtihad) or on authority grounds (who has the right to issue it) — the dominant axis of actual dispute indicates which framing the community itself is using.',
    'If the method framing dominates, this constraint sits closer to a rope with contested territory; if the authority-claim framing dominates, the tangled_rope classification with identity-locked victims is the more accurate reading, which is the framing adopted here based on the observed centrality of authority disputes in actual traditionalist-reformist polemics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination_kernel_vs_authority_claim, conceptual, 'Whether the constraint is best analyzed as a hermeneutic method or as a legitimacy claim over existing authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t8, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 8, 0.14).
narrative_ontology:measurement(qura_tr_t16, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 16, 0.17).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 24, 0.19).
narrative_ontology:measurement(qura_tr_t32, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 32, 0.21).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(qura_be_t8, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(qura_be_t16, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 24, 0.3).
narrative_ontology:measurement(qura_be_t32, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 32, 0.31).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t8, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(qura_su_t16, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 16, 0.33).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 24, 0.31).
narrative_ontology:measurement(qura_su_t32, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 32, 0.29).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__reformist_ijtihad, 0.1).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quran_hadith_substrate kernel. traditionalist_taqlid claims classical madhhab consensus is binding and taqlid obligatory (expected higher suppression, expected victims among reform-seeking populations). state_hybrid claims the state selectively adopts classical rulings in family/criminal law while applying secular/reformist frameworks elsewhere, with legitimacy grounded in sovereignty rather than doctrinal fidelity. Each reading has its own ε, its own beneficiary/victim structure, and its own claimed type — they are not measurement variants of one constraint but three structurally distinct constraints sharing a contested textual/doctrinal substrate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
