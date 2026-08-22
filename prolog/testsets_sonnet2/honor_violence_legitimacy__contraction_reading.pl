% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Code Redefinition Foreclosing Dueling (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION reading of the
 *   honor_violence_legitimacy kernel: dueling's decline is read here as a
 *   semantic/conceptual event — honor itself was redefined by courts, clergy,
 *   professionals, and cultural arbiters so that violent vindication exits
 *   the category of legitimate honor response altogether. Under this reading,
 *   dueling did not become rare because it grew costly (that is the sibling
 *   drop_reading) but became structurally UNTHINKABLE because the concept it
 *   depended on was hollowed out and replaced. The referent of epsilon here
 *   is the standing arrangement under contest as this reading sees it: the
 *   redefinition apparatus itself (courts, honor-vocabulary producers,
 *   professional-class norm entrepreneurs) which now functions mostly as
 *   settled inertial common sense — hence the piton classification. The
 *   composite_reading (external cost + conceptual redefinition operating
 *   together) and the drop_reading (practical rarity without conceptual
 *   foreclosure) are NOT represented in this file; they are separate
 *   constraints linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - bourgeois_professional_class: beneficiary of the new honor-grammar built around their existing non-violent probity norms
 *   - state_judicial_monopolists: agenda_setter administering the redefinition through law, courts, and honor tribunals
 *   - reformed_honor_arbiters: beneficiary/agenda_setter who author and profit culturally from the replacement vocabulary
 *   - residual_aristocratic_dueling_culture: payer, identity-locked into a grammar of honor that the redefinition dismantles from under them
 *   - military_officer_corps_traditionalists: payer, isolated within institutional subculture as civilian discourse contracts
 *   - legal_and_cultural_historians: analytical observer reconstructing the semantic shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.28).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.42).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Code Redefinition Foreclosing Dueling (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'c50d3d66-77e9-4566-bfbd-1306ef57f8c1').
narrative_ontology:cs_kernel_codification('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', distributed).
narrative_ontology:cs_authority_grounding('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', practice).
narrative_ontology:cs_interpretation_layer_present('c50d3d66-77e9-4566-bfbd-1306ef57f8c1').
narrative_ontology:cs_reading_relation('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', foundational, honor_requires_conceptual_exclusion_of_violence).
narrative_ontology:cs_axiom_status(honor_requires_conceptual_exclusion_of_violence, holdable).
narrative_ontology:cs_axiom_grounding('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', honor_requires_conceptual_exclusion_of_violence, conventional).
narrative_ontology:cs_axiom('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', secondary, violent_vindication_was_never_merely_costly_but_became_categorically_illegible).
narrative_ontology:cs_axiom_status(violent_vindication_was_never_merely_costly_but_became_categorically_illegible, holdable).
narrative_ontology:cs_axiom_grounding('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', violent_vindication_was_never_merely_costly_but_became_categorically_illegible, empirically_contingent).
narrative_ontology:cs_reference_frame('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', aristocratic_violence_vindicated_honor).
narrative_ontology:cs_drift_state('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', post_bourgeois_professionalization_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('c50d3d66-77e9-4566-bfbd-1306ef57f8c1', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_judicial_monopolists).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, reformed_honor_arbiters).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, residual_aristocratic_dueling_culture).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, military_officer_corps_traditionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rising professionals (lawyers, merchants, civil servants) whose social advancement depended on merit and reputation systems that did not require willingness to kill or die over insult. As honor is redefined to mean probity, self-restraint, and civic reputation rather than violent vindication, their existing behavioral repertoire becomes the model of honorable conduct rather than a deficient imitation of aristocratic manners.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).

% Courts, legislatures, and monarchical or republican administrations that campaign to redefine honor discourse itself — through law, sermon, press, and educational curricula — so that dueling no longer register as a coherent honor response at all, rather than merely punishing it as a crime. They administer the redefinition machinery: honor courts, satisfaction tribunals, published codes of gentlemanly conduct that omit violence entirely.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_judicial_monopolists, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Journalists, clergy, novelists, and etiquette authorities who construct the new vocabulary of honor — apology, retraction, litigation, social ostracism — and thereby become the recognized authorities on what honor requires. Their cultural authority is constituted by supplying the replacement grammar, and grows in direct proportion to how completely violence is expunged from the honor concept.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, reformed_honor_arbiters, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, reformed_honor_arbiters, agenda_setter).

% Aristocrats and gentry whose entire self-concept and claim to social precedence was built on willingness to answer insult with the sword or pistol. As the concept of honor is redefined around them, they do not merely lose a practical option — they find that the very vocabulary in which they understood their own dignity has been dismantled. Fighting a duel no longer reads as honorable; it reads as archaic, criminal, or faintly ridiculous. Their exit is blocked by identity, not by law: renouncing dueling means renouncing the only honor-grammar they were raised to inhabit.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, residual_aristocratic_dueling_culture, payer,
    powerful, biographical, identity_locked, national).

% Officers whose corps culture retained dueling longest as a marker of caste solidarity and battlefield courage credentialing. As civilian honor discourse contracts to exclude violence, they are increasingly isolated as an anachronistic subculture, subject to military regulations and civilian ridicule alike, without a replacement mechanism that carries the same weight of proof-of-courage inside their institution.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, military_officer_corps_traditionalists, payer,
    powerful, biographical, constrained, national).

% Individuals under the old regime who were compelled to accept challenges or lose all standing, often at fatal risk, had no voice in either the old system or its dismantling — they experienced coercion under the prior kernel-reading and are not present to attest whether the new arrangement is better or merely different. Their historical absence is structural: the redefinition project was conducted by elites, professionals, and clergy, not by past duel-compelled victims.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, dueling_challenge_recipients_historical, excluded,
    powerless, biographical, trapped, local).

% Scholars who trace the semantic history of 'honor' across legal codes, sermons, novels, and correspondence to assess whether dueling's decline reflects conceptual redefinition, external cost increase, or both. Their reconstructions are the primary evidence base for adjudicating between the contraction, drop, and composite readings of this kernel.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_and_cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, reformed_honor_arbiters).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, publicly legible standard of what counts as honorable conduct so that disputes over reputation can be resolved without escalating to lethal violence — coordinating expectations among elites, professionals, and the state about acceptable means of vindicating dignity.
% TRANSFER_FUNCTION: Moves social and interpretive authority over the concept of honor away from the dueling aristocracy and toward the state's judicial apparatus, the rising professional class, and the cultural arbiters (press, clergy, etiquette authorities) who author the replacement vocabulary. It also transfers physical risk away from potential duelists broadly, at the cost of the aristocratic dueling culture's claim to a distinctive, violence-proving form of status.
% ABSENT_VOICES: Historical individuals coerced into duels under the prior honor-grammar have no standing in the redefinition process — they are dead, and the redefinition project addresses the concept prospectively, not their historical injuries. Their absence means the case for contraction is argued entirely by parties who stood to gain from it (state, professionals, cultural arbiters), not by anyone harmed under the old regime.
% DISAPPEARANCE_RATIONALE: If the redefinition of honor to exclude violence were reversed overnight — if honor snapped back to require willingness to duel — significant portions of professional, legal, and civic life would have to reorganize around renewed risk of lethal violence over reputational disputes; courts, workplaces, and journalism would need entirely different conflict-resolution norms, and the modern professional class's status claims (built on non-violent probity) would lose their grounding.
% FOUNDING_PROBLEM: Elite society needed a mechanism to resolve disputes over insult and reputation that did not depend on private lethal combat, which was destabilizing to state monopoly on violence, costly in elite lives (including military officers), and increasingly incompatible with emerging bourgeois and legal-rational norms of civic order.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists of honor (e.g., studies of dueling's decline in 19th-century Britain, France, and the American South) attest from outside the beneficiary set that by the late 19th/early 20th century the practical problem of elite lethal dispute-resolution had been resolved through law, insurance-like reputational institutions, and shifting masculine norms — the redefinition of honor persists now primarily as settled cultural common sense rather than as an active solution to a live problem. No dueling-culture defender offers a counter-corroboration of the founding problem's persistence; the traditionalist officer corps sources instead contest the STATUS of the redefinition (calling it decline of virtue) rather than defending dueling as solving any current problem.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low-moderate (0.28 at interval end) because the contraction reading describes primarily a conceptual/discursive shift rather than a mechanism actively extracting resources from a clearly bounded victim class — the 'cost' borne by dueling culture is identity-dislocation, not material transfer. Suppression (0.42) reflects that the redefinition project used real institutional pressure (legal penalties, social ridicule, exclusion from professional respectability) to make the old honor-grammar unspeakable, but this is milder and more diffuse than a snare's coercive suppression. accessibility_collapse is high (0.72) because once honor is successfully redefined, dueling genuinely drops out of the legitimate option-space for most social actors — this is the contraction reading's central claim, and the metric should reflect it strongly. resistance is moderate (0.35): military and aristocratic holdouts resisted for decades, but their resistance dwindled as the conceptual replacement became naturalized. theater_ratio rises over the interval (0.08 to 0.30) because as the practical problem (elite lethal dispute resolution) recedes into history, the remaining honor-discourse apparatus (etiquette codes, honor courts, gentlemanly conduct manuals) increasingly performs vindication of a settled cultural consensus rather than solving any live coordination problem — this is the piton signature: administered by agenda-setters who could relax the discourse further, diffusely costly to no one in particular anymore, maintained mostly by inertia and institutional habit rather than active extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the state/professional/arbiter seats, the redefinition looks like successful civilizing coordination — a genuine reduction in elite lethal violence achieved through cultural progress. From the identity-locked aristocratic dueling seat, the same redefinition looks like the erasure of a coherent moral vocabulary they were raised inside, with no equivalent replacement offered on their own terms — not merely a cost imposed, but a conceptual rug pulled out. The engine should compute these divergently from the same structural data: the beneficiary seats see rope/coordination, the identity-locked payer seat experiences something closer to snare-flavored dispossession, even though no one is actively extracting rent from them going forward — hence the piton classification as the story's own claim, capturing that no concentrated beneficiary is currently profiting from residual dueling-culture displacement; the displacement mostly already happened and what remains is aftermath.
 *
 * DIRECTIONALITY LOGIC:
 *   state_judicial_monopolists and reformed_honor_arbiters sit near the beneficiary end: they gain interpretive authority and reduced governance burden from a pacified elite dispute-resolution landscape. bourgeois_professional_class benefits by having their pre-existing behavioral repertoire elevated to the new honor standard, at essentially no cost to them (d near full beneficiary). residual_aristocratic_dueling_culture and military_officer_corps_traditionalists are declared victims/payers because the redefinition specifically devalues the cultural capital they had accumulated in the old system; their identity_locked and constrained exit options (respectively) reflect that they cannot simply adopt the new honor-grammar without abandoning what made them distinctive, and by the time contraction is complete they have no alternative discourse-space left to retreat into.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (need for non-lethal elite dispute resolution) is authored as dead — it was solved, and solved thoroughly, decades to a century before the interval's end. What persists afterward — honor codes, etiquette manuals, honor-adjacent professional norms — increasingly does not solve a live coordination problem so much as ratify a settled cultural memory of having solved one. This is precisely the piton signature the classification is meant to catch: not a currently-extractive snare, and not a genuine ongoing rope (the coordination need it addresses has receded), but an inertial residue maintained by institutional and cultural habit. Naming this piton rather than mislabeling it rope prevents the classification from treating stale cultural performance as if it were still solving an active problem; naming it piton rather than snare prevents treating diffuse historical dispossession as if there were a current concentrated beneficiary actively profiting from continued suppression of dueling-honor vocabulary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_drop_discriminability,
    'Can the historical record actually discriminate between the contraction reading (honor''s meaning changed) and the drop reading (honor''s meaning stayed fixed but external costs rose), or do the surviving sources underdetermine which mechanism was operative?',
    'Close textual analysis of contemporaneous honor discourse (sermons, etiquette manuals, court records, private correspondence) for explicit redefinition language versus mere cost-complaint language; convergent evidence from multiple national contexts (Britain, France, Germany, American South) with different legal-cost trajectories would help isolate the conceptual mechanism from the cost mechanism.',
    'If sources show honor being explicitly redefined in normative vocabulary (dueling called dishonorable, not merely illegal or costly), the contraction reading is supported; if sources show dueling still described as honorable but merely impractical, the drop reading is supported; mixed evidence supports the composite reading and would suggest none of the three single-mechanism files fully captures the historical constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_drop_discriminability, empirical, 'Whether contraction is separable from drop given available historical evidence.').

omega_variable(
    redefinition_agency_versus_epiphenomenon,
    'Was the redefinition of honor an agentic project actively pursued by identifiable beneficiary groups (state, professional class, cultural arbiters), or was it an epiphenomenal byproduct of broader modernization processes (urbanization, rise of print culture, professionalization) that no one specifically engineered?',
    'Trace whether specific actors (legislators drafting anti-dueling statutes with explicit honor-redefinition rhetoric, etiquette authors explicitly repositioning honor vocabulary) can be shown to have intended the conceptual shift, versus whether the shift appears as an emergent aggregate effect with no identifiable authorial intent.',
    'If agentic, the beneficiary declarations (state_judicial_monopolists, reformed_honor_arbiters) are well-grounded and the piton/extraction framing is appropriate; if epiphenomenal, the beneficiary declarations overstate agency and the constraint may be closer to a mountain-adjacent structural drift than an authored redefinition project.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(redefinition_agency_versus_epiphenomenon, conceptual, 'Whether declared beneficiaries actively drove the redefinition or merely benefited from an emergent shift.').

omega_variable(
    identity_lock_dissolution_pathway,
    'For the identity-locked aristocratic dueling culture, did the identity fusion with violent honor-vindication actually dissolve gradually across generations, or did it persist underground (in alternative violence rituals, subcultural codes) after the mainstream discourse contracted?',
    'Trace successor rituals (fraternity hazing codes, military academy customs, underground fencing/dueling societies) for continuity of the violence-honor fusion after formal dueling''s decline.',
    'If the identity fusion persisted in displaced form, the contraction reading''s claim that dueling became ''structurally unthinkable'' is too strong — the underlying identity structure may have relocated rather than dissolved, which would favor a reading closer to composite or even suggest the kernel needs a fourth reading for displacement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_dissolution_pathway, empirical, 'Whether identity-locked violence-honor fusion dissolved or merely relocated after contraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__contraction_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__contraction_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__contraction_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(hono_tr_t80, honor_violence_legitimacy__contraction_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(hono_tr_t100, honor_violence_legitimacy__contraction_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__contraction_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__contraction_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__contraction_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(hono_be_t80, honor_violence_legitimacy__contraction_reading, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(hono_be_t100, honor_violence_legitimacy__contraction_reading, base_extractiveness, 100, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_violence_legitimacy__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the honor_violence_legitimacy kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: contraction_reading (this file, piton — conceptual redefinition foreclosing dueling from the honor category), drop_reading (dueling remains legitimate in principle but practically abandoned due to rising external costs — likely scaffold or rope depending on whether the cost-imposition itself is read as coordination or extraction), and composite_reading (both mechanisms operating together, likely tangled_rope given the interaction of genuine coordination benefit with the extraction of cultural capital from displaced aristocratic actors). All three share the same underlying historical phenomenon (dueling's decline) but instantiate structurally distinct claims about WHY it declined, with different beneficiary/victim structures and different ε values, and must not be merged into a single file with an averaged or hedged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
