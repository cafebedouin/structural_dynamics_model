% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Textualist-Traditionist Reading of the Jurisprudential Method Kernel
 *   domain: Islamic Jurisprudence / Legal Philosophy / Institutional History
 *
 * SUMMARY:
 *   This story instantiates the Hanbali reading of the jurisprudential method
 *   kernel: law derives strictly from the literal text of Qur'an and Hadith
 *   and from Companion opinion; analogical reasoning (qiyas) and juristic
 *   preference (istihsan) are treated as bid'ah — innovations that corrupt
 *   the kernel — and only unanimous scholarly consensus (ijma) counts as a
 *   valid supplementary source. This is one of four sibling readings of a
 *   single contested kernel (Hanafi, Maliki, Shafi'i being the others); each
 *   is authored as its own constraint with its own stable epsilon, per the
 *   ε-invariance principle. The Hanbali reading is distinguished by the
 *   severity of its exclusion: where Hanafi treats qiyas as a legitimate
 *   extension tool and Shafi'i institutionalizes it as the fourth tier of a
 *   formal hierarchy, this reading brands the same reasoning practice as
 *   corruption of the divine kernel itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.71).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.68).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Textualist-Traditionist Reading of the Jurisprudential Method Kernel").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "Islamic Jurisprudence / Legal Philosophy / Institutional History").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, 'c9da0340-fa30-4530-acb1-dfea084f3806').
narrative_ontology:cs_kernel_codification('c9da0340-fa30-4530-acb1-dfea084f3806', fixed_text).
narrative_ontology:cs_authority_grounding('c9da0340-fa30-4530-acb1-dfea084f3806', lineage).
narrative_ontology:cs_interpretation_layer_present('c9da0340-fa30-4530-acb1-dfea084f3806').
narrative_ontology:cs_reading_relation('c9da0340-fa30-4530-acb1-dfea084f3806', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('c9da0340-fa30-4530-acb1-dfea084f3806', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9da0340-fa30-4530-acb1-dfea084f3806', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_axiom('c9da0340-fa30-4530-acb1-dfea084f3806', foundational, qiyas_is_corrupting_innovation).
narrative_ontology:cs_axiom_status(qiyas_is_corrupting_innovation, holdable).
narrative_ontology:cs_axiom_grounding('c9da0340-fa30-4530-acb1-dfea084f3806', qiyas_is_corrupting_innovation, deontological).
narrative_ontology:cs_axiom('c9da0340-fa30-4530-acb1-dfea084f3806', foundational, only_unanimous_consensus_validates_ruling).
narrative_ontology:cs_axiom_status(only_unanimous_consensus_validates_ruling, holdable).
narrative_ontology:cs_axiom_grounding('c9da0340-fa30-4530-acb1-dfea084f3806', only_unanimous_consensus_validates_ruling, conventional).
narrative_ontology:cs_reference_frame('c9da0340-fa30-4530-acb1-dfea084f3806', companion_era_textual_transmission).
narrative_ontology:cs_drift_state('c9da0340-fa30-4530-acb1-dfea084f3806', post_classical_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c9da0340-fa30-4530-acb1-dfea084f3806', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_hadith_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, traditionist_authority_networks).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, local_qadis_using_juristic_preference).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, textual_literalism_doctrine).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, companion_precedent_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compile and transmit hadith corpora, adjudicate isnad reliability, and declare which rulings count as valid derivations from text and Companion opinion. Their authority rests entirely on being the recognized gatekeepers of textual and traditionist knowledge; the more the method excludes analogical reasoning and juristic preference as illegitimate, the more indispensable their specific expertise in hadith criticism becomes. They administer the standard and are structurally positioned to benefit from its narrowness.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_hadith_scholars, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, textualist_hadith_scholars, beneficiary).

% Teaching circles and scholarly lineages built around transmission of hadith and Companion reports. They gain prestige, students, and patronage precisely because the reading treats their specialized transmission chains as the only legitimate route to law, foreclosing competing routes (analogy, local custom, juristic discretion) that would distribute authority more widely.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, traditionist_authority_networks, beneficiary,
    organized, generational, mobile, regional).

% Jurists trained in analogical reasoning (qiyas) and juristic preference (istihsan) who extend textual rulings to novel cases via reasoned extrapolation. Under this reading their entire methodological toolkit is branded bid'ah — innovation that corrupts the kernel. They can relocate to jurisdictions or schools that accept their method, but within Hanbali-dominant institutions their rulings are delegitimized and their standing as authoritative jurists is foreclosed.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, regional).

% Communities whose inherited local practice ('urf) has functioned as a practical source of dispute resolution for generations. This reading treats such custom as having no independent evidentiary weight unless it can be traced to unanimous scholarly consensus or direct textual warrant — an all but impossible bar. They cannot relocate their entire community's accumulated practice; they simply lose recognition for what they already do.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities, payer,
    powerless, biographical, trapped, local).

% Judges who historically resolved gaps in textual coverage through istihsan — choosing the more equitable of two analogically valid rulings. Under this reading their discretion has no method status; any ruling not directly traceable to text, Companion opinion, or unanimous consensus is vulnerable to being overturned or denounced as innovation, which strips them of interpretive latitude in ambiguous cases.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, local_qadis_using_juristic_preference, payer,
    moderate, biographical, constrained, local).

% Rival schools whose entire methodological apparatus (qiyas, 'amal ahl al-Madina, the four-tier hierarchy) is what this reading labels corrupting innovation. They are not consulted within Hanbali-administered courts and their objections — that reason is a legitimate tool for extending divine intent, or that living community practice preserves prophetic norms — are treated as the very problem the kernel must exclude, not as competing valid readings to be weighed.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanafi_maliki_shafii_scholarly_networks, excluded,
    organized, generational, mobile, regional).

% Trace how the unanimous-consensus requirement, combined with rejection of qiyas, functioned historically to concentrate interpretive authority in scholars capable of hadith criticism while narrowing the range of cases that could be adjudicated at all, producing documented tension with administrators needing rulings on novel commercial and administrative questions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, legal_historians_of_islamic_law, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, textualist_hadith_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rigorously bounded, traceable chain of legal authority — every ruling must trace to Qur'an, authenticated Hadith, or Companion opinion — which protects against the proliferation of arbitrary, unaccountable rulings issued under the banner of reasoned extension.
% TRANSFER_FUNCTION: Moves interpretive authority and the social capital that accompanies it away from jurists relying on analogy, juristic preference, or local custom, and concentrates it in scholars whose expertise is hadith transmission and textual literalism; it also shifts the burden of legal uncertainty onto litigants and communities whose situations are not directly addressed by extant text.
% ABSENT_VOICES: Rationalist jurists, Maliki scholars invoking Medinan communal practice, and ordinary communities relying on customary dispute-resolution norms would object that the reading treats necessary tools for handling novel cases as illegitimate corruption; they are not part of the consensus this reading requires, and their exclusion is precisely how the unanimity bar functions to foreclose them.
% DISAPPEARANCE_RATIONALE: If this reading's authority collapsed, jurists across the excluded schools would resume analogical reasoning and juristic preference without needing to defend against the bid'ah charge, local customary practice would regain evidentiary standing in dispute resolution, and the textualist scholarly networks whose prestige depends on being sole gatekeepers of authenticated transmission would lose a substantial share of their institutional position.
% FOUNDING_PROBLEM: Early juristic reasoning (qiyas, ra'y, istihsan) was seen by some traditionists as opening the law to unconstrained personal opinion, risking corruption of divine intent by fallible human speculation, particularly as legal questions multiplied faster than explicit textual coverage.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars themselves attest the problem (unconstrained ra'y) remains live and requires continued exclusion of analogy. Historians of Islamic law and jurists from the Hanafi, Maliki, and Shafi'i traditions — outside the benefiting textualist network — argue the underlying problem (preventing arbitrary rulings) was addressed centuries ago by structured methodologies like qiyas and the four-source hierarchy, and that the unanimity requirement now functions primarily to concentrate authority rather than to solve an active problem; no source outside the Hanbali transmission networks corroborates that qiyas itself remains a live danger.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) because the reading's practical effect is to funnel interpretive authority — and the social, judicial, and economic capital attached to it — toward a narrow class of hadith-transmission specialists, while foreclosing an entire methodological toolkit (qiyas, istihsan, 'urf) used by rival jurists and by ordinary communities to resolve novel disputes. Suppression is authored moderately high (0.68) because the mechanism is not merely persuasive: rulings issued via analogy or custom are actively delegitimized as bid'ah, a status with real institutional consequences (loss of judicial standing, exclusion from consensus-building bodies). Theater ratio is kept comparatively low (0.28) because the hadith-criticism apparatus this reading depends on is a genuinely functioning, technically rigorous scholarly practice, not mere performance — the extraction rides on real methodological labor, which is what makes it durable rather than merely decorative.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist hadith scholars and the traditionist networks around them are the structural beneficiaries: their specific technical competence (isnad criticism, hadith authentication) is what the reading declares indispensable, and their institutional position is enhanced precisely by narrowing the field of legitimate method to exclude competitors. Rationalist jurists, customary-practice communities, and qadis exercising juristic preference are the victims: their tools are branded corruption, their rulings delegitimized, and — for the customary-practice communities especially — their exit options are essentially nil (an entire community cannot relocate its inherited practice to a friendlier jurisdiction), which places them near the trapped end of exit_options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unconstrained personal opinion (ra'y) corrupting divine law — was a genuine methodological concern in early Islamic jurisprudence. But the corroboration record shows the problem was substantially addressed by the very methodological disciplines (structured qiyas, the Shafi'i four-tier hierarchy) that this reading continues to reject wholesale. The founding_problem_status is marked contested rather than dead because textualist scholars sincerely maintain the danger is live; but no source outside their own transmission networks corroborates that stance, which is exactly the asymmetry the mandatrophy question is designed to expose — a constraint whose justification is affirmed only by those it empowers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading (hanbali_reading) of the jurisprudential_method_kernel, contested against hanafi_reading, maliki_reading, and shafii_reading. Where precisely is the disagreement located — is it about the SOURCES of law, the METHOD of extending sources to novel cases, or the STANDARD of consensus required to validate a ruling?',
    'Comparative doctrinal analysis across the four schools'' usul al-fiqh (legal theory) treatises shows the disagreement is layered: all four accept Qur''an and Hadith as primary; the split begins at whether reasoned extension (qiyas) is legitimate at all (Hanbali says no in principle, though later Hanbali practice softened this considerably), and sharpens further at what counts as valid consensus (unanimous vs. scholarly-community agreement).',
    'If the disagreement is purely about consensus threshold (unanimous vs. majority), the readings could in principle converge with looser evidentiary standards for ijma; if it is about the legitimacy of reasoned extension itself, the readings are structurally irreconcilable, which changes the sibling relationship from influences to something closer to genuine methodological foreclosure for specific case classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Locating the precise axis of disagreement among the four sibling readings of the jurisprudential method kernel.').

omega_variable(
    historical_hanbali_practice_vs_doctrine,
    'Later historical Hanbali jurisprudence (including figures like Ibn Taymiyyah and Ibn Qayyim) is documented as using forms of reasoned inference and maslaha (public interest) considerations that function similarly to qiyas in practice, despite the school''s doctrinal rejection of analogy in principle. Is the reading authored here (strict textual literalism, wholesale rejection of qiyas) an accurate description of the kernel as historically practiced, or a description of its stated doctrine that diverges from lived jurisprudential practice?',
    'Close reading of actual Hanbali legal opinions (fatawa) across centuries, compared against the school''s own stated usul al-fiqh, to measure the gap between declared method and practiced method.',
    'If practice diverges substantially from doctrine, the authored extractiveness and suppression may overstate the reading''s actual operation — the theater_ratio should rise to capture doctrine-practice divergence, and some of what looks like extraction may be doctrinal positioning rather than functioning method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_hanbali_practice_vs_doctrine, empirical, 'Whether the strict anti-qiyas doctrine matches documented Hanbali juristic practice or diverges from it.').

omega_variable(
    textualism_natural_vs_constructed,
    'Is the claim that only literal text and Companion opinion constitute valid law a genuine epistemic constraint (the only reliable route to recovering divine intent, given the risk of human error in reasoning) or a constructed methodological choice that happens to concentrate authority among scholars with hadith-transmission expertise?',
    'Comparative reliability analysis: does the unanimous-consensus-plus-literal-text method actually produce lower error rates in legal outcomes than qiyas-based reasoning, or does it primarily produce narrower coverage (more unresolved cases) with authority concentrated among fewer scholars?',
    'If genuinely epistemically superior for the goal of preserving divine intent, the tangled_rope classification understates a real coordination function; if the narrower coverage is the actual effect with authority concentration as the actual outcome, this supports the extraction reading over the naturalness framing implicit in the reading''s own self-justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textualism_natural_vs_constructed, conceptual, 'Whether strict textualism is a superior epistemic method or a constructed authority-concentration mechanism dressed as epistemic caution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 60, 0.67).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 80, 0.66).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language concept 'the source-and-method question in Islamic jurisprudence' into four structurally distinct readings of a single contested kernel (jurisprudential_method_kernel). Each reading has its own epsilon, its own beneficiary/victim structure, and its own classification: hanbali_reading (this story) is authored as tangled_rope with high epsilon (0.71) reflecting rejection of qiyas and a strict unanimity requirement; hanafi_reading is expected to show substantially lower epsilon given its embrace of reasoned extension as a coordination tool; maliki_reading centers a different beneficiary (Medinan traditionists) via its living-practice source; shafii_reading formalizes a middle path via its four-tier hierarchy. All four are linked bidirectionally via affects_constraints since they compete for the same jurisdictional and doctrinal space historically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
