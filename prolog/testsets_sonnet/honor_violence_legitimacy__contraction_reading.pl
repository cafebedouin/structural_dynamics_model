% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefined to Exclude Violence (Contraction Reading of the Honor-Violence Kernel)
 *   domain: historical_sociology/legal_anthropology
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION reading of the
 *   honor_violence_legitimacy kernel: dueling did not merely become rare or
 *   costly (that is the sibling drop_reading's claim), it became structurally
 *   unthinkable because the concept of honor itself was redefined to exclude
 *   violent vindication as a category member. Under this reading, by the late
 *   nineteenth century a gentleman who dueled was not taking an increasingly
 *   risky honorable action — he was acting outside the honor concept
 *   entirely, comparable to someone claiming honor through theft. This is a
 *   conceptual-space contraction, not a cost-benefit shift. The
 *   composite_reading (external cost escalation AND conceptual redefinition
 *   operating together) and the drop_reading (dueling stayed legitimate but
 *   became rare due to legal risk, dueling bans, insurance-like social costs)
 *   are separate constraint stories with their own ε values — they are NOT
 *   alternative measurements of this same constraint, per the ε-invariance
 *   principle. Each reading has a different beneficiary/victim structure:
 *   under the drop_reading, dueling participants remain honor-legitimate
 *   actors merely facing higher costs, so there is no 'victim of
 *   redefinition' in the same sense.
 *
 * KEY AGENTS:
 *   - bourgeois_professional_class: primary beneficiary of the new honor vocabulary — organized, mobile, generational horizon
 *   - the_state_judicial_monopoly: agenda-setter administering the redefinition through law and doctrine — institutional, civilizational horizon
 *   - womens_moral_reform_networks: beneficiary of expanded moral authority despite lacking coercive power
 *   - aristocratic_officer_caste: primary payer — loses categorical access to the honor-violence response, trapped by identity investment
 *   - traditional_dueling_seconds: secondary payer — entire institutional role rendered obsolete
 *   - historians_of_honor_culture: analytical observer weighing this reading against drop_reading and composite_reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.28).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.35).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefined to Exclude Violence (Contraction Reading of the Honor-Violence Kernel)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'acec9233-d7c0-43b3-8c04-48feebbde6e7').
narrative_ontology:cs_kernel_codification('acec9233-d7c0-43b3-8c04-48feebbde6e7', distributed).
narrative_ontology:cs_authority_grounding('acec9233-d7c0-43b3-8c04-48feebbde6e7', practice).
narrative_ontology:cs_interpretation_layer_present('acec9233-d7c0-43b3-8c04-48feebbde6e7').
narrative_ontology:cs_reading_relation('acec9233-d7c0-43b3-8c04-48feebbde6e7', honor_violence_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('acec9233-d7c0-43b3-8c04-48feebbde6e7', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('acec9233-d7c0-43b3-8c04-48feebbde6e7', foundational, violence_is_conceptually_incompatible_with_honorable_conduct).
narrative_ontology:cs_axiom_status(violence_is_conceptually_incompatible_with_honorable_conduct, holdable).
narrative_ontology:cs_axiom_grounding('acec9233-d7c0-43b3-8c04-48feebbde6e7', violence_is_conceptually_incompatible_with_honorable_conduct, conventional).
narrative_ontology:cs_axiom('acec9233-d7c0-43b3-8c04-48feebbde6e7', secondary, honorable_status_is_demonstrated_through_self_restraint_not_martial_risk).
narrative_ontology:cs_axiom_status(honorable_status_is_demonstrated_through_self_restraint_not_martial_risk, holdable).
narrative_ontology:cs_axiom_grounding('acec9233-d7c0-43b3-8c04-48feebbde6e7', honorable_status_is_demonstrated_through_self_restraint_not_martial_risk, conventional).
narrative_ontology:cs_reference_frame('acec9233-d7c0-43b3-8c04-48feebbde6e7', aristocratic_martial_honor_code).
narrative_ontology:cs_drift_state('acec9233-d7c0-43b3-8c04-48feebbde6e7', late_nineteenth_century_bourgeois_consolidation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('acec9233-d7c0-43b3-8c04-48feebbde6e7', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, the_state_judicial_monopoly).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, womens_moral_reform_networks).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, aristocratic_officer_caste).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, traditional_dueling_seconds).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, honor_is_conceptually_separable_from_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, civility_norms_constitute_legitimate_honor_defense).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rising professionals (lawyers, physicians, merchants, civil servants) whose social advancement depended on demonstrating respectability through self-restraint rather than martial display. As honor's meaning shifted toward reputation-for-probity, they gained a status vocabulary that let them claim honor without needing dueling's aristocratic training, weapons, or leisure time. They actively promoted courts of honor, print condemnation of duelists, and legal reform.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).

% Centralizing states had long wanted a monopoly on legitimate violence; dueling was a standing rival claim that private gentlemen could adjudicate their own insults by force. As honor was redefined discursively (in law codes, sermons, newspapers, courtesy literature) to exclude violent vindication, the state's criminalization of dueling stopped looking like an attack on honor and started looking like honor's natural ally. The state administered the redefinition through statute, prosecution policy, and army regulations rather than through ongoing coercion against believers.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, the_state_judicial_monopoly, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Wives, mothers, and reform-minded writers who bore the collateral costs of dueling (widowhood, orphaned children, ruined estates) had no formal standing to prohibit it, but gained real influence once honor's content was redefined toward domestic virtue and self-command. Their moral authority over the new honor vocabulary rose even though they held no coercive power and could not have stopped a duel by force.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, womens_moral_reform_networks, beneficiary,
    organized, generational, constrained, national).

% Military officers and old nobility whose entire status grammar had been built on the duel as the ultimate honor-vindicating act. As the conceptual redefinition took hold, they did not merely lose a practical option — they lost the categorical availability of the response they had been raised to consider constitutive of manhood and rank. Continuing to duel after the redefinition marked one as backward or criminal rather than honorable; there was no adjacent vocabulary left to retreat into.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, aristocratic_officer_caste, payer,
    powerful, biographical, trapped, national).

% The intermediaries whose social function was to negotiate, witness, and legitimate duels lost their entire institutional role once dueling exited the category of honorable action. Their expertise (dueling codes, procedural etiquette, arms selection) became worthless status capital almost overnight, with no transitional accommodation.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, traditional_dueling_seconds, payer,
    moderate, biographical, trapped, regional).

% Scholars reconstructing why dueling declined, weighing whether the decisive mechanism was conceptual redefinition (this reading), external cost escalation (the drop reading), or both operating together (the composite reading). They examine sermons, courtesy manuals, legal codes, and press coverage to trace whether 'honor' itself changed meaning or merely became more expensive to defend violently.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, historians_of_honor_culture, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, stable vocabulary for what counts as honorable conduct so that social actors can recognize, praise, and punish behavior without each dispute escalating into private violence — honor as a coordination device for status competition that does not require weapons.
% TRANSFER_FUNCTION: Moves status-conferring authority away from those skilled in violence (dueling aristocrats, seconds, arms masters) toward those skilled in self-restraint, legal recourse, and reputational management (professionals, clergy, reform writers, and the state's courts).
% ABSENT_VOICES: The dying dueling culture itself has no institutional voice left to object once the redefinition completes — by the time the conceptual shift is legible as a shift, the aristocratic officer caste that would contest it has already been recategorized as anachronistic rather than as a legitimate dissenting party.
% DISAPPEARANCE_RATIONALE: If the redefinition unwound and honor reverted to including violent vindication as a legitimate response, dueling would not merely become common again by habit — the entire apparatus of courts of honor, professional codes of conduct, and legal treatment of assault-in-defense-of-reputation would have to reorganize around violence as a live honor option once more.
% FOUNDING_PROBLEM: Endemic elite violence: duels killed disproportionately valuable political, military, and administrative personnel, destabilized state monopoly on legitimate force, and made honor disputes a recurring source of unregulated bloodshed that no single institution could fully suppress by force alone.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and demographers studying elite mortality records attest that lethal dueling among officers and gentry had become statistically rare and socially unthinkable by the period the redefinition is dated to complete — this is corroborated by court-martial records and press archives external to the reform networks that promoted the redefinition, not merely by the reformers' own self-congratulatory tracts.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).
:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 by 1900) because the contraction reading describes a genuine conceptual coordination achievement — reducing elite lethal violence — rather than a rent-extraction mechanism; what extraction exists is the status-transfer from aristocratic violence-skill to bourgeois self-restraint-skill, a real but modest redistribution of honor-capital. Suppression (0.35) reflects that the mechanism operates primarily through redefinition and social sanction (shaming, exclusion from polite society, legal prosecution) rather than through continuous coercive enforcement — once the concept shifts, most compliance is definitional rather than policed. Accessibility collapse is authored high (0.82) precisely because this IS the contraction reading's defining claim: once honor is redefined, dueling does not remain an available-but-costly alternative (that would be the drop reading) — it exits the legitimate option set almost completely for those who accept the new honor concept. Resistance is authored moderate-low (0.3): the aristocratic caste resisted, but resistance was culturally overwhelmed rather than actively suppressed by force.
 *
 * PERSPECTIVAL GAP:
 *   From the state's and bourgeois professional's seat, the redefinition looks like Rope: a genuine coordination gain (fewer elite deaths, a stable non-violent status vocabulary) with modest, diffuse extraction. From the aristocratic officer caste's seat, the same redefinition looks like something closer to Snare or Tangled Rope: their entire status-grammar was expropriated and there is no adjacent legitimate category to retreat into — the coordination benefit accrues to a different population than the one bearing the concentrated cost of category loss.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (bourgeois professionals, the state, reform networks) sit near the beneficiary end of directionality because the redefinition directly increases their status capital and institutional reach at low personal cost. The aristocratic officer caste and dueling seconds sit near the full-target end: they are identity-locked (their entire honor-grammar was built around violent vindication) and trapped (there is no alternative honor vocabulary they can retreat into that preserves their prior status claims) — this is why their exit_options are authored as trapped rather than merely constrained. This is a stronger claim than the drop_reading would make about the same population, where dueling elites merely face rising costs but retain conceptual legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (endemic elite violence destabilizing state authority) is authored as dead by 1900 by evidence external to the reform networks (mortality records, court-martial archives), which prevents the classification from over-crediting the reformers' own self-narrative. The arrangement is not classified as mandatrophic because the underlying coordination structure (a stable, non-violent honor vocabulary) persisted and continued performing its function after the crisis passed, rather than persisting as pure theater after its function died — theater_ratio rises only modestly (to 0.22) reflecting some performative reputation-management residue, not wholesale drift into pure ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_drop_discriminability,
    'Can the historical record actually discriminate between conceptual redefinition (this reading) and mere cost escalation (drop_reading) as the operative mechanism, or do both readings predict the same observable decline curve in dueling frequency?',
    'Examine whether contemporaries who ceased dueling described their abstention in terms of honor no longer permitting violence (contraction-consistent language) versus in terms of legal/social risk outweighing benefit while still affirming dueling''s honorableness in principle (drop-consistent language). Courtesy literature, private correspondence, and trial testimony from the period are the relevant corpus.',
    'If the historical language is overwhelmingly cost-based rather than category-based, this contraction_reading story is empirically weaker than its claimed_type suggests and the drop_reading sibling would be the better-supported constraint for this population; if category-based language dominates, this reading is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_drop_discriminability, empirical, 'Whether the historical record supports conceptual contraction versus mere cost-based decline.').

omega_variable(
    redefinition_engineered_or_emergent,
    'Was the redefinition of honor a deliberately engineered project (by the state and bourgeois reformers, for their own status advancement) or an emergent cultural drift that those groups merely benefited from after the fact?',
    'Trace whether courts-of-honor legislation and courtesy-manual campaigns preceded or followed observable declines in dueling frequency; deliberate engineering would show coordinated institutional action preceding the behavioral shift.',
    'If engineered, the beneficiary structure authored here (bourgeois professionals, the state) is a stronger causal claim and the constraint leans more clearly toward tangled_rope (coordination cover for a status-capture project); if emergent, the rope classification is better supported and the beneficiary relationship is more incidental than causal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(redefinition_engineered_or_emergent, conceptual, 'Whether beneficiary groups authored the redefinition or merely rode a spontaneous cultural shift.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between the three kernel readings live — is it a factual dispute about which causal mechanism dominated, or an irreducible framing choice about what counts as ''honor changing meaning'' versus ''honor''s application changing''?',
    'This is likely partially irreducible: the composite_reading''s own existence suggests professional historians treat the mechanisms as empirically entangled rather than crisply separable, which would mean the contraction/drop distinction is partly a matter of analytical emphasis rather than a fact that could be settled by any single archive.',
    'If the boundary is genuinely a framing choice rather than a factual dispute, this story''s claimed_type and metrics should be read as one legitimate analytical lens among the three, not as the uniquely correct historical account — which is consistent with how the kernel/reading structure is meant to be used.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the contraction/drop distinction is empirically resolvable or an irreducible framing choice within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__contraction_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(hono_tr_t1780, honor_violence_legitimacy__contraction_reading, theater_ratio, 1780, 0.1).
narrative_ontology:measurement(hono_tr_t1810, honor_violence_legitimacy__contraction_reading, theater_ratio, 1810, 0.13).
narrative_ontology:measurement(hono_tr_t1840, honor_violence_legitimacy__contraction_reading, theater_ratio, 1840, 0.17).
narrative_ontology:measurement(hono_tr_t1870, honor_violence_legitimacy__contraction_reading, theater_ratio, 1870, 0.2).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.22).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1750, 0.12).
narrative_ontology:measurement(hono_be_t1780, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1780, 0.15).
narrative_ontology:measurement(hono_be_t1810, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1810, 0.19).
narrative_ontology:measurement(hono_be_t1840, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1840, 0.23).
narrative_ontology:measurement(hono_be_t1870, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1870, 0.26).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_violence_legitimacy__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__contraction_reading, 0.1).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the honor_violence_legitimacy kernel, decomposed per the ε-invariance principle rather than authored as a single constraint with an observable-dependent classification. contraction_reading claims a high accessibility_collapse (0.82) reflecting categorical exclusion of violence from legitimate honor; drop_reading would claim a lower accessibility_collapse reflecting a still-available-but-costly option; composite_reading treats both mechanisms as empirically entangled. All three share the same historical population (dueling elites, reformers, the state) but assign different ε, different accessibility_collapse, and different exit_options to the same nominal agents based on which causal reading is authored. The reading_relations in cs_structure declare contraction_reading FORECLOSES drop_reading (their core premises about whether the category itself changed are mutually exclusive within a single framework) while INFLUENCING composite_reading (which absorbs contraction as one of its two claimed mechanisms without being logically ruled out by it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
