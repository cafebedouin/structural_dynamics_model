% ============================================================================
% CONSTRAINT STORY: nineteenth_amendment__full_citizenship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nineteenth_amendment__full_citizenship_reading, []).

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
 *   constraint_id: nineteenth_amendment__full_citizenship_reading
 *   human_readable: Nineteenth Amendment as Full Citizenship Structural Commitment
 *   domain: constitutional_law/sex_equality
 *
 * SUMMARY:
 *   The Nineteenth Amendment ('The right of citizens of the United States to
 *   vote shall not be denied or abridged by the United States or by any State
 *   on account of sex') instantiates a contested kernel that permits two
 *   structurally distinct readings. This constraint story generates ONE
 *   reading: the full citizenship reading, which holds that the Amendment
 *   stands for women's full and equal citizenship as a structural commitment
 *   that should inform sex-equality interpretation across the entire
 *   Constitution. The sibling reading — the suffrage-only reading — holds
 *   that the Amendment did exactly what it says: enfranchised women and left
 *   every other legal disability of sex untouched. These are not mere
 *   interpretive disagreements; they are competing frameworks that produce
 *   different classification structures. The full-citizenship reading
 *   generates extractiveness from the suppression of its own implementing
 *   force — minimalist doctrine actively constrains the reading's scope. The
 *   suffrage-only reading generates extractiveness through institutional
 *   inertia and the preservation of sex-hierarchical legal structures. From
 *   the full-citizenship framework, the constraint is a Tangled Rope: genuine
 *   coordination function (the Amendment solves enfranchisement) coupled with
 *   asymmetric extraction (minimalism suppresses the broader structural
 *   implication). From the suffrage-only framework (the sibling), the
 *   constraint would be classified as Rope: pure coordination with no
 *   suppressed implication. The engine's false-summit detector will flag the
 *   textualist 'mountain' perspective as naturalization of a particular
 *   interpretive theory, not discovery of textual immutability.
 *
 * KEY AGENTS:
 *   - Sex-Equality Jurisprudence Coalition: Organized advocates (feminist legal scholars, civil rights organizations, sympathetic judges) — beneficiary of full-citizenship reading as doctrinal foundation; constrained by institutional resistance
 *   - Minimalist Institutional Doctrine: Entrenched common law and constitutional tradition — beneficiary of suffrage-only reading; maintains institutional continuity and preserves hierarchy-protective legal structures
 *   - Women as Constitutional Subjects: Structurally positioned as victims under minimalism (trapped without exit from sex subordination through constitutional doctrine)
 *   - Progressive Constitutional Courts: Institutional agents adopting full-citizenship frame (rope perspective) — benefit from having interpretive frame that explains doctrinal inconsistency
 *   - Textualist Observer: Analytical position (mountain perspective, later flagged as false summit) — naturalizes suffrage-only reading as textual necessity rather than interpretive choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nineteenth_amendment__full_citizenship_reading, 0.38).
domain_priors:suppression_score(nineteenth_amendment__full_citizenship_reading, 0.52).
domain_priors:theater_ratio(nineteenth_amendment__full_citizenship_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nineteenth_amendment__full_citizenship_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(nineteenth_amendment__full_citizenship_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(nineteenth_amendment__full_citizenship_reading, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nineteenth_amendment__full_citizenship_reading, tangled_rope).
narrative_ontology:human_readable(nineteenth_amendment__full_citizenship_reading, "Nineteenth Amendment as Full Citizenship Structural Commitment").
narrative_ontology:topic_domain(nineteenth_amendment__full_citizenship_reading, "constitutional_law/sex_equality").

domain_priors:requires_active_enforcement(nineteenth_amendment__full_citizenship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nineteenth_amendment__full_citizenship_reading, '61092e96-fd05-4400-a068-0207de2963e7').
narrative_ontology:cs_kernel_codification('61092e96-fd05-4400-a068-0207de2963e7', fixed_text).
narrative_ontology:cs_authority_grounding('61092e96-fd05-4400-a068-0207de2963e7', lineage).
narrative_ontology:cs_interpretation_layer_present('61092e96-fd05-4400-a068-0207de2963e7').
narrative_ontology:cs_reading_relation('61092e96-fd05-4400-a068-0207de2963e7', nineteenth_amendment__suffrage_only_reading, coexists_with).
narrative_ontology:cs_axiom('61092e96-fd05-4400-a068-0207de2963e7', foundational, enfranchisement_implies_equal_citizenship).
narrative_ontology:cs_axiom_status(enfranchisement_implies_equal_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('61092e96-fd05-4400-a068-0207de2963e7', enfranchisement_implies_equal_citizenship, deontological).
narrative_ontology:cs_axiom('61092e96-fd05-4400-a068-0207de2963e7', foundational, structural_subordination_incompatible_with_citizenship).
narrative_ontology:cs_axiom_status(structural_subordination_incompatible_with_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('61092e96-fd05-4400-a068-0207de2963e7', structural_subordination_incompatible_with_citizenship, deontological).
narrative_ontology:cs_reference_frame('61092e96-fd05-4400-a068-0207de2963e7', women_as_equal_citizens).
narrative_ontology:cs_drift_state('61092e96-fd05-4400-a068-0207de2963e7', contemporary_legal_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('61092e96-fd05-4400-a068-0207de2963e7', '').
narrative_ontology:cs_kernel_id(nineteenth_amendment__full_citizenship_reading, nineteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nineteenth_amendment__full_citizenship_reading, sex_equality_jurisprudence).
narrative_ontology:constraint_victim(nineteenth_amendment__full_citizenship_reading, suffrage_only_minimalism).
narrative_ontology:constraint_victim(nineteenth_amendment__full_citizenship_reading, sex_subordination_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN UNDER SUFFRAGE-ONLY MINIMALISM (SNARE) — Trapped within the suffrage-only interpretive frame, which grants the ballot but leaves sex-based legal disabilities untouched (marital property rules, employment discrimination, reproductive autonomy, protective legislation that masks subordination). No exit from sex hierarchy because the minimalist reading forecloses a structural remedy. Maximum experienced extraction: the amendment's promise is systematically suppressed to the single dimension of voting rights.
constraint_indexing:constraint_classification(nineteenth_amendment__full_citizenship_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SEX-EQUALITY JURISPRUDENCE COALITION (TANGLED ROPE) — Organized advocates (feminist legal scholars, civil rights organizations, sympathetic judges) benefit from the full-citizenship reading as a doctrinal foundation for structural sex-equality claims. But also constrained: even with the broader constitutional frame, implementing the reading requires overcoming institutional resistance, entrenched common law, and competing constitutional narratives. Mixed coordination and extraction: the reading enables coalition action (rope function) while the suppression of the reading's force by minimalist doctrine (snare mechanism) constrains implementation.
constraint_indexing:constraint_classification(nineteenth_amendment__full_citizenship_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROGRESSIVE CONSTITUTIONAL COURTS (ROPE) — Courts adopting the full-citizenship reading experience it as a coordination mechanism: it solves the interpretive problem of reconciling the Amendment's text with persistent sex-based subordination by reading the subordination as constitutionally disfavored. Low extractiveness; courts benefit from having an interpretive frame that explains and remedies doctrinal inconsistency. The reading enables coherent jurisprudence.
constraint_indexing:constraint_classification(nineteenth_amendment__full_citizenship_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MINIMALIST INSTITUTIONAL DOCTRINE (PITON) — The suffrage-only reading persists as institutional precedent and common-law tradition despite the full-citizenship reading's logical and historical challenge. High theater ratio: the minimalist reading maintains performative legitimacy through textualist arguments ('the Amendment says enfranchisement, nothing more') even as the social and constitutional context has moved beyond the 1920 horizon. The reading is inert institutional inheritance, maintained by citation chains and conservatism rather than functional necessity.
constraint_indexing:constraint_classification(nineteenth_amendment__full_citizenship_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NEUTRAL TEXTUALIST OBSERVER (MOUNTAIN) — From a strict textualist perspective, the Nineteenth Amendment is immutable on its face: 'The right of citizens of the United States to vote shall not be denied or abridged by the United States or by any State on account of sex.' The text is what it is; the text says enfranchisement; no broader commitment flows from those words. This perspective naturalizes the suffrage-only reading as a logical consequence of textual immutability. However, the engine will flag this as a false summit: the claim that the text immutably forecloses the full-citizenship reading presupposes a particular theory of constitutional interpretation, not a neutral discovery.
constraint_indexing:constraint_classification(nineteenth_amendment__full_citizenship_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FULL CITIZENSHIP READING (TANGLED ROPE) — From a civilizational view that treats the Nineteenth Amendment as a kernel commitment to women's full and equal citizenship, the constraint structure becomes visible: the Amendment coordinated a doctrinal solution to enfranchisement (rope function) while the suffrage-only reading suppresses the broader structural commitment (snare function). The full-citizenship reading creates interpretive pressure on minimalist doctrine, triggering institutional resistance. The analytical observer at this position sees both the coordination benefit (clarity about constitutional values) and the extraction mechanism (suppression of the reading's implementing force).
constraint_indexing:constraint_classification(nineteenth_amendment__full_citizenship_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nineteenth_amendment__full_citizenship_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nineteenth_amendment__full_citizenship_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nineteenth_amendment__full_citizenship_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(nineteenth_amendment__full_citizenship_reading, TR),
    TR >= 0.70.

:- end_tests(nineteenth_amendment__full_citizenship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.38): Moderate. The full-citizenship reading itself generates no direct extraction — it is a doctrinal frame that enables sex-equality claims. The extractiveness value represents the suppression of the reading's force by minimalist doctrine. Minimalism extracts value from preserving sex-based legal structures (marital property regimes, reproductive control, employment hierarchies) that historically depended on treating women as less-than-equal citizens. The reading's suppression enables that extraction to continue. The value is moderate rather than high because: (1) the full-citizenship frame is now live in significant portions of constitutional jurisprudence (though not dominant); (2) the suppression mechanisms are doctrinal rather than physical (no direct coercion, but interpretive constraints); (3) the reading has substantial scholarly and institutional support. SUPPRESSION (0.52): Moderate-high. The full-citizenship reading faces significant suppressions: textualist canons (literal reading confined to enfranchisement), stare decisis (the suffrage-only precedents), institutional conservatism (courts reluctant to invalidate sex-based legal structures), and competing constitutional narratives (federalism, separation of powers). But suppression is not total — the reading is live in doctrine, explicitly adopted in some domains (some sex-discrimination jurisprudence), and actively developed by organized advocates. THEATER_RATIO (0.41): Moderate. The minimalist reading has performative elements (textualist arguments that treat narrow enfranchisement as immutable), but the constraint itself is not primarily theatrical. The sex-hierarchical legal structures that minimalism preserves are functionally embedded in property law, family law, and employment regulation — they are not merely symbolic. The theater_ratio has declined over the interval (from 0.58 to 0.41) because the factual implausibility of pure enfranchisement-only (women vote but retain most legal disabilities) has become increasingly evident through subsequent jurisprudence, making the textualist defense less performatively convincing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full perspectival range. Women trapped under minimalism perceive the constraint as a Snare: the Amendment's promise is suppressed to a single dimension (voting) while sex subordination persists across law. Sex-equality advocates perceive a Tangled Rope: the Amendment genuinely coordinated enfranchisement while its broader implications remain suppressed and contested. Progressive courts perceive a Rope: the full-citizenship reading solves the interpretive problem of explaining why voting rights but not equal citizenship? Minimalist doctrine persists as a Piton: the suffrage-only reading is maintained through institutional inertia and textualist ritual despite the growing implausibility of its foundational claim. The textualist observer risks perceiving a Mountain (textual immutability) but the engine flags this as a false summit — textualism is an interpretive theory, not a neutral discovery of what the text permits. The massive perspectival gap — ranging from Snare through Rope, Tangled Rope, and Piton to Mountain — reflects that the kernel contest is not a dispute about a single fact but a dispute about the very structure of constitutional meaning.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) captures the agent's structural relationship to the full-citizenship reading as a constraint. Powerless women trapped under minimalism experience maximum extraction: the reading's suppression leaves them without constitutional remedy for sex subordination beyond voting (d ≈ 0.95). Organized sex-equality advocates benefit from the reading but are constrained by institutional resistance (d ≈ 0.40, moderate). Progressive courts adopting the full-citizenship frame experience it as a coordination benefit — it solves the interpretive problem of reconciling the Amendment with persistent sex-based legal structures (d ≈ 0.15, low extraction). The minimalist institutional doctrine (piton perspective) has beneficiary access to preservation of hierarchical legal structures but is constrained by the increasing implausibility of the suffrage-only reading (d ≈ 0.25, modest extraction). The textualist observer position (flagged as false summit) derives d from analytical position and the claim that textual immutability is neutral rather than interpretively constructed (canonical d for analytical ≈ 0.73).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that both readings are institutionally live but serve different structural functions. The full-citizenship reading enables sex-equality doctrine; its force is constrained by minimalist doctrine that preserves sex-hierarchical legal structures. The suffrage-only reading preserves institutional continuity and prevents broad constitutional reconstruction. The constraint is not that one reading is 'correct' — it is that the readings compete at the level of fundamental constitutional meaning. Adopting the full-citizenship reading would require recognizing suppression of the Amendment's implications as constitutionally anomalous; adopting the suffrage-only reading requires accepting that enfranchisement was the only structural commitment and that sex subordination in every other domain is constitutional. The mandatrophy resolves into an institutional choice: which reading will dominate the interpretive landscape? Currently, mixed: the full-citizenship frame is live but not dominant. The extraction (0.38) and suppression (0.52) values reflect this mixed institutional state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_anchor_indeterminacy,
    'Does the text of the Nineteenth Amendment anchor ONLY the suffrage-only reading, or is the full-citizenship reading a legitimate inference from the structural commitment to non-discrimination the Amendment embodies?',
    'Historical analysis of framing intent, concurrent state constitutional provisions, and the Amendment''s subsequent interpretive trajectory. Comparison of textualist canons (original public meaning vs. semantic scope).',
    'If suffrage-only is textually determinate: full-citizenship reading is a creative doctrine requiring strong policy justification; suppression of it is defensible minimalism. If text is open to broader construction: minimalism is an active choice to constrict the Amendment''s force, not a neutral reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_anchor_indeterminacy, conceptual, 'Whether the Amendment''s text determines suffrage-only or permits full-citizenship reading').

omega_variable(
    structural_implication_scope,
    'If enfranchisement is a foundational right of citizenship, what other legal disabilities of sex follow structurally as denials of equal citizenship?',
    'Doctrine trace: which sex-based legal rules (marital property, employment, reproduction, criminal justice) have been identified as inconsistent with full citizenship in post-Amendment jurisprudence? Comparative analysis: which state and federal courts have adopted the full-citizenship frame, and which legal domains have it reshaped?',
    'If scope is narrow (only voting-adjacent): full-citizenship reading has limited extractive force; minimalism constrains only narrow class of claims. If scope is broad (structural prohibition on sex subordination): full-citizenship reading is highly constraining; minimalism suppresses significant doctrinal reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_implication_scope, empirical, 'Structural scope of equal-citizenship implication for sex-based legal disabilities').

omega_variable(
    reading_foreclosure_asymmetry,
    'Does the full-citizenship reading logically foreclose the suffrage-only reading, or do they coexist as competing interpretive frameworks within constitutional law?',
    'Logical analysis: can a constitutional interpreter consistently hold both that (a) the Amendment guarantees enfranchisement AND that (b) other sex-based legal disabilities are not constitutionally prohibited? Or does commitment to full citizenship as a structural principle necessarily exclude minimalist confinement to voting alone?',
    'If forecloses: the two readings are genuinely incompatible; adoption of full-citizenship reading resolves the kernel contest. If coexist: both remain live positions held by different institutional actors; the constraint is a coexistence mechanism for competing readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_asymmetry, conceptual, 'Logical relationship between full-citizenship and suffrage-only readings').

omega_variable(
    institutional_resistance_mechanism,
    'Is the persistence of the suffrage-only reading driven by textualist interpretive principle, or by institutional reluctance to adopt a broader reading that would require dismantling entrenched sex-based legal structures?',
    'Comparative jurisprudence: do courts that adopt textualism consistently apply the same hermeneutic to other constitutional provisions, or is textualism selectively deployed to narrow the Nineteenth Amendment? Analysis of judicial rhetoric: how do minimalist opinions justify constricting the Amendment''s scope?',
    'If principled textualism: the suffrage-only reading has epistemic grounding; full-citizenship advocates must address interpretive theory. If selective institutional constraint: the suppression of the full-citizenship reading is extractive mechanism (snare function), not neutral doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_resistance_mechanism, empirical, 'Institutional sources of minimalist reading persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nineteenth_amendment__full_citizenship_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(n19_full_cit_theater_t0, nineteenth_amendment__full_citizenship_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement(n19_full_cit_theater_t20, nineteenth_amendment__full_citizenship_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(n19_full_cit_theater_t50, nineteenth_amendment__full_citizenship_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(n19_full_cit_extract_t0, nineteenth_amendment__full_citizenship_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(n19_full_cit_extract_t20, nineteenth_amendment__full_citizenship_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(n19_full_cit_extract_t50, nineteenth_amendment__full_citizenship_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(n19_full_cit_suppress_t0, nineteenth_amendment__full_citizenship_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(n19_full_cit_suppress_t20, nineteenth_amendment__full_citizenship_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(n19_full_cit_suppress_t50, nineteenth_amendment__full_citizenship_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nineteenth_amendment__full_citizenship_reading, identity_coordination).
narrative_ontology:affects_constraint(nineteenth_amendment__full_citizenship_reading, nineteenth_amendment__suffrage_only_reading).
narrative_ontology:affects_constraint(nineteenth_amendment__full_citizenship_reading, sex_equality_doctrine_scope).
narrative_ontology:affects_constraint(nineteenth_amendment__full_citizenship_reading, equal_protection_sex_classification_scrutiny).

% DUAL FORMULATION NOTE:
% The Nineteenth Amendment instantiates a kernel that permits multiple readings. The full-citizenship reading and the suffrage-only reading are structurally distinct constraints with different ε values, different beneficiary/victim sets, and different classification profiles. The full-citizenship reading generates extractiveness from the suppression of its own implementing force. The suffrage-only reading generates extractiveness through institutional preservation of sex-hierarchical structures. Each reading is modeled as a separate constraint story; this file captures the full-citizenship reading. The network edge to suffrage_only_reading marks the kernel relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
