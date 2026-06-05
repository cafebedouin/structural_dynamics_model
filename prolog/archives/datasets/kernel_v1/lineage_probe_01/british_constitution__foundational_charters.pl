% ============================================================================
% CONSTRAINT STORY: british_constitution__foundational_charters
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_british_constitution__foundational_charters, []).

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
 *   constraint_id: british_constitution__foundational_charters
 *   human_readable: British Constitution: Foundational Charters Reading
 *   domain: political/legal
 *
 * SUMMARY:
 *   The foundational_charters reading asserts that the British constitution
 *   is grounded in medieval and early-modern documents — Magna Carta (1215),
 *   the Petition of Right (1628), the Bill of Rights (1689) — through which
 *   the baronage and then the commons extracted limits on royal prerogative
 *   incrementally. Each charter was a tactical concession forced from the
 *   Crown under duress or negotiating pressure; each represented a bargained
 *   reduction in the Crown's claimed power to rule arbitrarily. The
 *   constraint is tangled_rope: the charters coordinate the Crown's authority
 *   (by establishing written rules) while enabling asymmetric extraction (the
 *   Crown retains prerogative power and repeatedly reinterprets or revokes
 *   charter grants). The extractiveness declines over time (0.85 → 0.38) as
 *   charter enforcement mechanisms shift from baronial resistance to
 *   parliamentary consent to, eventually, judicial review and convention. The
 *   theater_ratio rises (0.30 → 0.48) as the 'ancient constitution' narrative
 *   solidifies — later generations retroactively reinterpret tactical grants
 *   as expressions of fundamental law rather than as coerced concessions.
 *
 * KEY AGENTS:
 *   - The Baronage (12th-14th centuries): Primary extractors of charter concessions; constrained exit (cannot abandon Crown entirely) but organized power to withhold cooperation
 *   - The Commons/Parliament (14th-17th centuries): Secondary extractors leveraging taxation consent; constrained exit but increasing organizing capacity
 *   - The Crown (institutional): Retains prerogative and arbitrage options; experiences charters as coordination mechanism enabling legitimacy
 *   - The Subject Class (powerless): Theoretically protected by charter grants but bears full cost of prerogative reassertions; trapped exit
 *   - The Constitutional Continuity Narrative (16th-17th centuries onward): Reinterprets tactical documents as expressions of fundamental law; piton perspective
 *   - The Analytical Observer (civilizational): Risks naturalizing the contested institutional achievement of charter enforcement as inherent to all governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(british_constitution__foundational_charters, 0.38).
domain_priors:suppression_score(british_constitution__foundational_charters, 0.62).
domain_priors:theater_ratio(british_constitution__foundational_charters, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(british_constitution__foundational_charters, extractiveness, 0.38).
narrative_ontology:constraint_metric(british_constitution__foundational_charters, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(british_constitution__foundational_charters, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(british_constitution__foundational_charters, tangled_rope).
narrative_ontology:human_readable(british_constitution__foundational_charters, "British Constitution: Foundational Charters Reading").
narrative_ontology:topic_domain(british_constitution__foundational_charters, "political/legal").

domain_priors:requires_active_enforcement(british_constitution__foundational_charters).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(british_constitution__foundational_charters, '9cb6cdd9-74bc-4a8e-8e90-c1d735189788').
narrative_ontology:cs_kernel_codification('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', formalized).
narrative_ontology:cs_authority_grounding('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', lineage).
narrative_ontology:cs_interpretation_layer_present('9cb6cdd9-74bc-4a8e-8e90-c1d735189788').
narrative_ontology:cs_reading_relation('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', british_constitution__constitutional_conventions, coexists_with).
narrative_ontology:cs_reading_relation('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', british_constitution__parliamentary_supremacy_statutes, influences).
narrative_ontology:cs_reading_relation('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', british_constitution__revolution_settlement, coexists_with).
narrative_ontology:cs_reading_relation('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', british_constitution__modern_judicialization, influences).
narrative_ontology:cs_axiom('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', foundational, chartered_liberty_incrementally_extracted).
narrative_ontology:cs_axiom_status(chartered_liberty_incrementally_extracted, holdable).
narrative_ontology:cs_axiom_grounding('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', chartered_liberty_incrementally_extracted, empirically_contingent).
narrative_ontology:cs_axiom('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', foundational, lineage_continuity_of_charter_authority).
narrative_ontology:cs_axiom_status(lineage_continuity_of_charter_authority, holdable).
narrative_ontology:cs_axiom_grounding('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', lineage_continuity_of_charter_authority, conventional).
narrative_ontology:cs_reference_frame('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', charter_extraction_model).
narrative_ontology:cs_drift_state('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9cb6cdd9-74bc-4a8e-8e90-c1d735189788', '').
narrative_ontology:cs_kernel_id(british_constitution__foundational_charters, british_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(british_constitution__foundational_charters, baronage_and_commons).
narrative_ontology:constraint_beneficiary(british_constitution__foundational_charters, emerging_subject_class).
narrative_ontology:constraint_victim(british_constitution__foundational_charters, unconstrained_kingship).
narrative_ontology:constraint_victim(british_constitution__foundational_charters, royal_prerogative).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMONER SUBJECT (SNARE) — Before charter extraction, the powerless subject faces arbitrary rule with no exit. Prerogative allows the Crown to tax without consent, conscript without justification, and punish without due process. The charter grants (Magna Carta, Bill of Rights) create formal limits, but enforcement is persistently undermined by prerogative reassertion. The subject experiences the constraint as pure extraction: the Crown retains the power to override its own written commitments.
constraint_indexing:constraint_classification(british_constitution__foundational_charters, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BARONAGE (TANGLED ROPE) — The magnates who extract the charters experience genuine coordination: they need a Crown to coordinate defense and justice. But they also experience extraction: the Crown's prerogative power means each grant must be defended anew in each reign. The charters are a coordination mechanism (establishing shared rules) combined with asymmetric extraction (the Crown can revoke or reinterpret at will). The baronage's constrained exit reflects that they cannot abandon the Crown entirely but can withdraw cooperation, creating negotiating leverage.
constraint_indexing:constraint_classification(british_constitution__foundational_charters, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CROWN (ROPE) — From the Crown's institutional perspective, the charters are coordination mechanisms for rule. The Crown benefits from subjects' willing compliance more than from arbitrary extraction — legitimate authority requires perceived consent. The charters enable this: they signal that rule is bounded and thus worth accepting. The Crown experiences low effective extraction because it uses charter language to justify its own authority. Arbitrage options reflect the Crown's capacity to reinterpret charters or withdraw from constraint selectively.
constraint_indexing:constraint_classification(british_constitution__foundational_charters, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARLIAMENT/COMMONS (TANGLED ROPE) — As Parliament accumulates power to consent to taxation and petition for redress, the constraint becomes actively enforced: Parliament can withhold supply or refuse cooperation. The commons experience genuine coordination (taxation requires consent, reducing arbitrary extraction) combined with asymmetric extraction (the Crown retains prerogative in war, appointments, and law-making). The organized commons has more capacity to enforce charter limits than dispersed subjects, but parliament's own sovereignty remains contested and partial.
constraint_indexing:constraint_classification(british_constitution__foundational_charters, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL CONTINUITY NARRATIVE (PITON) — The account that 'the British constitution is a continuous line from Magna Carta to present' is substantially performative. The charters were tactical documents extracted under duress; many were annulled by the Crown within years. The 'constitutional' reading of Magna Carta as fundamental law is a 16th-17th century retroactive interpretation, not the document's original function. The continuity narrative persists through institutional inertia and ceremonial invocation despite the historical record showing repeated rupture and reconstitution. Theater ratio reflects the gap between the 'ancient constitution' mythology and the actual punctuated history of charter reinterpretation.
constraint_indexing:constraint_classification(british_constitution__foundational_charters, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some constraint on arbitrary executive power is inherent to sustainable governance: all functioning states eventually develop limits on ruler power, whether through charters, norms, or revolution. The charter extraction appears as an inevitable structural feature of how power is domesticated, not a contingent institutional arrangement. However, the beneficiary declarations and the tight coupling between charter revision and political crisis suggest this is a false summit: the 'inevitability' of charter limits naturalizes what is actually a contested, repeatedly-ruptured institutional achievement.
constraint_indexing:constraint_classification(british_constitution__foundational_charters, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(british_constitution__foundational_charters_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(british_constitution__foundational_charters, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(british_constitution__foundational_charters, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(british_constitution__foundational_charters, TR),
    TR >= 0.70.

:- end_tests(british_constitution__foundational_charters_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint measures the degree to which arbitrary prerogative persists despite charter limits. Initial extractiveness is very high (0.85 at Magna Carta) because the Crown routinely violates or revokes charter grants; the constraint has minimal enforcement. Over time, extractiveness declines as enforcement mechanisms strengthen: parliamentary consent to taxation (1628 onward) and eventual conventions of prerogative restraint reduce the Crown's practical power to extract arbitrarily. The 0.38 final value reflects that by the 18th century, charter-derived limits are largely stable and enforced through political (rather than military) resistance. Suppression (0.62): Moderate-high. The mechanisms preventing exit or alternatives include: (1) the Crown's monopoly on legal authority and force; (2) the tacit nature of much prerogative power (exercised through appointment, interpretation, and discretion rather than explicit command); (3) the dependence of magnates and commons on Crown for defense and order; (4) the temporal gap between charter grants and enforcement (delays allow prerogative reassertion). Theater ratio (0.48): Moderate. The charters begin as relatively transparent negotiating documents (low theater at 0.30) but acquire increasing ceremonial and mythological weight as the 'ancient constitution' narrative develops. By the 18th century, the continuity of the charter tradition is more important than the actual content of specific rights — the charters are invoked as symbols of constitutional principle rather than as binding legal texts. The theater increase reflects the gap between the documents' original function (tactical concessions under duress) and their later interpretation (expressions of fundamental law).
 *
 * PERSPECTIVAL GAP:
 *   The foundational_charters reading produces strong perspectival gaps. The powerless subject sees a snare (charters fail to constrain prerogative in practice). The baronage sees tangled_rope (genuine coordination mechanism shadowed by Crown's ability to revoke). The Crown sees rope (charters enable rule legitimacy). Parliament sees tangled_rope (taxation consent enforces charter limits but prerogative persists in other domains). The continuity narrative sees piton (the charters persist through ceremonial invocation despite losing functional constraint). The analytical observer risks seeing mountain (prerogative limits are inevitable in all governance) — but the structural data reveals this as false summit: the limits are contingent on organized resistance by economically powerful classes, not inherent to kingship.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint derives directionality from the beneficiary-victim structure. The baronage and commons benefit from charter extraction (they accumulate written limits on prerogative), yielding them low d values (beneficiaries experience negative or low chi). The Crown experiences mixed directionality: it benefits from charter legitimacy (written limits justify rule) but loses prerogative scope, yielding a moderate d. The subject class bears extraction costs (no protections from arbitrary prerogative despite written charters) and has no exit, yielding high d (maximum experienced extraction, maximum chi). The analytical observer at the civilizational scope faces the paradox that the constraints appear to work (prerogative does eventually stabilize) but the mechanism is political struggle rather than legal binding, suggesting the mountain (natural law) view is misleading.
 *
 * MANDATROPHY ANALYSIS:
 *   The foundational_charters reading resolves mandatrophy by showing that the charters function as BOTH coordination mechanism AND extraction limit simultaneously. The coordination function is real: the Crown does coordinate defense and order through charter-bounded authority. The extraction function is also real: the Crown retains prerogative power and repeatedly reasserts it. Neither function can be reduced to the other. The tangled_rope classification captures both: the charters establish shared rules (rope) while enabling the Crown to override those rules (snare). The perspectival gap shows that the mandatrophy is not ambiguous at the structural level — it is a genuine hybrid — but becomes ambiguous when filtered through particular institutional positions (the Crown naturally emphasizes coordination; the subject naturally emphasizes extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_genuine_constraint_or_propaganda,
    'Do the medieval charters represent genuine legal constraints on Crown power, or are they primarily propaganda documents whose effectiveness depends entirely on political conditions?',
    'Historical analysis of Crown compliance with charter terms; correlation between charter provisions and documented prerogative actions; examination of annulment and reissue cycles. Comparison of charter-era prerogative assertions with post-charter-era ones.',
    'If genuine constraint: extractiveness drops to 0.20-0.25 (rope classification throughout). If propaganda: extractiveness remains 0.38+ (charters are performative rituals that fail to constrain). The reading''s core claim that charters ''extracted liberty'' depends on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_genuine_constraint_or_propaganda, empirical, 'Whether charters represent genuine legal constraints or performative political documents').

omega_variable(
    charter_reinterpretation_vs_continuity,
    'Is the ''Magna Carta as fundamental law'' interpretation (16th-17th century coke/whig narrative) a discovery of the document''s original meaning, or a deliberate reframing that transforms a tactical concession into constitutional bedrock?',
    'Textual analysis comparing original charter language and stated purpose (1215, 1265) with Stuart-era invocations (Coke, Selden); examination of whether 16th-century interpreters claimed to discover existing meaning or to create new meaning; identification of what would falsify the continuity narrative.',
    'If discovery: the reading instantiates genuine constitutional continuity; extractiveness interpretation stable. If reframing: the reading obscures the contingent, politically-motivated reconstruction of the charter tradition; suggests theater_ratio is underestimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_reinterpretation_vs_continuity, conceptual, 'Whether charter-as-fundamental-law is discovery or deliberate reframing').

omega_variable(
    enforcer_identity_shift,
    'Does the constraint''s enforcer shift from baronage (13th century) to commons/parliament (17th century) to judiciary (20th century), or do these represent different constraints with different extractiveness values?',
    'Test the ε-invariance principle: measure extractiveness relative to each enforcer. If d-values (directionality from each enforcer''s perspective) yield significantly different chi values, the constraint decomposes into separate stories. If chi values remain consistent across enforcers despite different d, the constraint maintains identity across structural shifts.',
    'If decomposition warranted: this story represents only the chartered-grant-extraction mechanism (13th-17th centuries); successor stories model parliamentary enforcement and judicial supremacy separately. If unified: the constraint''s core mechanism (written limits on prerogative) persists despite enforcer change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcer_identity_shift, empirical, 'Whether changing enforcers (baronage → parliament → judiciary) represent one constraint or multiple').

omega_variable(
    reading_kernel_contest_natural_law_vs_constructed,
    'Is the foundational_charters reading one legitimate interpretation of a contested kernel (the British constitution is multivalent, different readings coexist), or does this reading''s assertion of charter primacy foreclose the alternatives?',
    'Examine whether the foundational_charters reading logically excludes the constitutional_conventions reading (they could coexist: both charters AND conventions are operative, neither precludes the other) vs. whether it forecloses the revolution_settlement reading (if charters are foundational, are the 1688-1701 statutes merely implementation or do they supersede charter authority?). Map the logical structure of the kernel contest.',
    'If coexistent: the foundational_charters and constitutional_conventions readings both hold simultaneously; the kernel is genuinely multivalent. If foreclosed: the foundational_charters reading asserts an exclusive claim to constitutional authority that contradicts at least one sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest_natural_law_vs_constructed, conceptual, 'Whether this reading coexists with siblings or forecloses them logically').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(british_constitution__foundational_charters, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1215_tactical_grant, british_constitution__foundational_charters, theater_ratio, 0, 0.3).
narrative_ontology:measurement(theater_1265_reinterpretation_begins, british_constitution__foundational_charters, theater_ratio, 3, 0.38).
narrative_ontology:measurement(theater_1628_rights_language, british_constitution__foundational_charters, theater_ratio, 6, 0.45).
narrative_ontology:measurement(theater_1688_fundamental_law_claim, british_constitution__foundational_charters, theater_ratio, 8, 0.5).
narrative_ontology:measurement(theater_1707_stable_continuity_myth, british_constitution__foundational_charters, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(extractiveness_1215_magna_carta, british_constitution__foundational_charters, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(extractiveness_1265_barons_war, british_constitution__foundational_charters, base_extractiveness, 3, 0.72).
narrative_ontology:measurement(extractiveness_1628_petition_right, british_constitution__foundational_charters, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(extractiveness_1688_bill_of_rights, british_constitution__foundational_charters, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(extractiveness_1707_union_act, british_constitution__foundational_charters, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(british_constitution__foundational_charters, enforcement_mechanism).
narrative_ontology:affects_constraint(british_constitution__foundational_charters, british_constitution__constitutional_conventions).
narrative_ontology:affects_constraint(british_constitution__foundational_charters, british_constitution__parliamentary_supremacy_statutes).
narrative_ontology:affects_constraint(british_constitution__foundational_charters, british_constitution__revolution_settlement).
narrative_ontology:affects_constraint(british_constitution__foundational_charters, british_constitution__modern_judicialization).

% DUAL FORMULATION NOTE:
% The foundational_charters reading is one of five structurally distinct constraint stories in the british_constitution kernel family. All five readings have different epsilon values reflecting their different empirical bases: charters (ε~0.38, documentary basis, contestation over enforcement), conventions (ε~0.45+, unwritten, dependent on political will), parliamentary statutes (ε~0.30, formally binding, most recent), revolution settlement (ε~0.22, foundational reform, treated as immutable), and judicialization (ε~0.15, formal adjudication, most constraining). The family links show how each reading influences the others: charters provide the historical authority that statutes claim to express; conventions supplement written charters when they prove insufficient; the revolution settlement is often invoked as the true founding that displaces charter antiquity; judicialization formalizes what was previously divided between charters and conventions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(british_constitution__foundational_charters, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
