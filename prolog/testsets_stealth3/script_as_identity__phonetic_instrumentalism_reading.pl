% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__phonetic_instrumentalism_reading, []).

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
 *   constraint_id: script_as_identity__phonetic_instrumentalism_reading
 *   human_readable: Turkish Latin-Alphabet Standard (Phonetic-Instrumentalist Reading)
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   In 1928 the Grand National Assembly replaced the Arabic-derived Ottoman
 *   abjad with a Latin-letter alphabet for Turkish, legislated the change,
 *   barred Arabic characters from officialdom and printing, and ran mass
 *   evening courses (Millet Mektepleri) to convert the population. This story
 *   instantiates the phonetic-instrumentalist account of the resulting
 *   arrangement: the alphabet is treated as a neutral encoding technology
 *   selected on phonetic-engineering grounds — eight explicit vowel marks
 *   matching Turkish's harmonic vowel system — and the standing orthographic
 *   regime is assessed as a mature information standard. On this reading the
 *   arrangement's extraction is low: nothing is levied through the
 *   letterforms themselves, and the heavy costs that fell on the
 *   Arabic-script literate class were transition costs of a technical
 *   migration — severe, permanent for the individuals concerned, but not
 *   design-targeted at them. The story authors that low-ε assessment honestly
 *   while preserving, in omega variables, the questions this framing does not
 *   answer. KEY AGENTS (by structural relationship): -
 *   turkish_state_administration: Agenda-setter (institutional/arbitrage) —
 *   legislated, enforced, and still administers the standard; principal
 *   accrual seat - turkish_language_reformers: Designer-beneficiary
 *   (organized/mobile) — built the letter inventory and careers on it -
 *   post_reform_school_generations: Primary beneficiary
 *   (powerless/constrained) — inherit cheap literacy, no memory of the
 *   alternative - latin_script_printing_industry: Secondary beneficiary
 *   (moderate/mobile) — retooled early, holds the print market -
 *   ottoman_script_literate_class: Primary payer (organized/identity_locked)
 *   — textual capital devalued by statute; exit meant abandoning scholarly
 *   identity - adult_population_of_1928: Payer (powerless/trapped) — literacy
 *   investment stranded mid-passage -
 *   arabic_script_calligraphers_and_foundries: Payer (moderate/trapped) —
 *   patronage and type stock evaporated -
 *   rural_women_of_transition_generation: Excluded voice (powerless/trapped)
 *   — bore the campaign's blind spot - international_comparative_linguists:
 *   Analytical observer (analytical/analytical) — adjudicate the phonetic-fit
 *   claim from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.1).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.06).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, rope).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "Turkish Latin-Alphabet Standard (Phonetic-Instrumentalist Reading)").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__phonetic_instrumentalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, '8fb7184a-d712-4aba-a69f-0f7e824d210a').
narrative_ontology:cs_kernel_codification('8fb7184a-d712-4aba-a69f-0f7e824d210a', formalized).
narrative_ontology:cs_authority_grounding('8fb7184a-d712-4aba-a69f-0f7e824d210a', expertise).
narrative_ontology:cs_interpretation_layer_present('8fb7184a-d712-4aba-a69f-0f7e824d210a').
narrative_ontology:cs_reading_relation('8fb7184a-d712-4aba-a69f-0f7e824d210a', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fb7184a-d712-4aba-a69f-0f7e824d210a', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_axiom('8fb7184a-d712-4aba-a69f-0f7e824d210a', foundational, orthographic_form_is_neutral_technology).
narrative_ontology:cs_axiom_status(orthographic_form_is_neutral_technology, holdable).
narrative_ontology:cs_axiom_grounding('8fb7184a-d712-4aba-a69f-0f7e824d210a', orthographic_form_is_neutral_technology, empirically_contingent).
narrative_ontology:cs_axiom('8fb7184a-d712-4aba-a69f-0f7e824d210a', foundational, latin_vowel_notation_fits_turkish_harmony).
narrative_ontology:cs_axiom_status(latin_vowel_notation_fits_turkish_harmony, holdable).
narrative_ontology:cs_axiom_grounding('8fb7184a-d712-4aba-a69f-0f7e824d210a', latin_vowel_notation_fits_turkish_harmony, empirically_contingent).
narrative_ontology:cs_reference_frame('8fb7184a-d712-4aba-a69f-0f7e824d210a', phonetically_optimal_neutral_encoding).
narrative_ontology:cs_drift_state('8fb7184a-d712-4aba-a69f-0f7e824d210a', contemporary_sociolinguistic_era, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('8fb7184a-d712-4aba-a69f-0f7e824d210a', '').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, turkish_state_administration).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, turkish_language_reformers).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, post_reform_school_generations).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, latin_script_printing_industry).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, ottoman_script_literate_class).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, adult_population_of_1928).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, arabic_script_calligraphers_and_foundries).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, latin_script_phonetic_fitness_for_turkish).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, romanization_accelerates_mass_literacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislated the 1928 alphabet change, barred Arabic characters from official documents and printing, and funded the nationwide night-school campaign that taught the new letters. Controls school curricula, examinations, and the official orthography guide. Gains uniform administrative records, a literate conscript pool and electorate, and the symbolic standing of a modernized state. Amended letter shapes twice in the following decade and can revise the standard at will; no external body constrains that revision.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, turkish_state_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Linguists and committee members who designed the letter inventory and argued for the change in press and parliament. Secured founding roles in the state language institution created afterward, published the orthography guides, and built careers defending the reform. Their expertise is portable; several trained abroad.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, turkish_language_reformers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__phonetic_instrumentalism_reading, turkish_language_reformers, agenda_setter).

% Everyone schooled after the change learned an alphabet whose eight vowels are each written explicitly, with no consonant-context guessing. Reading acquisition is comparatively fast, and the script connects to the Latin typographic world of keyboards, catalogs, and international publication. Leaving the standard would mean privately acquiring another script at personal cost.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, post_reform_school_generations, beneficiary,
    powerless, generational, constrained, national).

% Type founders, press suppliers, and textbook printers who retooled for Latin letterforms in the 1930s and held the state's large print contracts thereafter. Capital and technique are movable; the domestic market's continued use of the standard is their customer base.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, latin_script_printing_industry, beneficiary,
    moderate, biographical, mobile, national).

% Ulema, scribes, jurists, poets, and calligraphers trained from childhood in the Arabic-letter Ottoman canon. Their reading skill, archives, and professional crafts lost official currency within months; their libraries passed to descendants unable to read them. Retraining meant years of evening courses mid-career and stepping away from a scholarly identity built on manuscript mastery. Organized through mosque networks and professional guilds at the time of the change, politically defeated within it.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_script_literate_class, payer,
    organized, generational, identity_locked, national).

% Roughly nine in ten adults could read neither script in 1928; those with Arabic-letter literacy, mostly townsmen, were offered voluntary evening courses few could attend while working. Existing literacy, where they had any, depreciated to private and family use; most finished their lives reading neither script fluently.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, adult_population_of_1928, payer,
    powerless, biographical, trapped, national).

% Master calligraphers lost state and religious patronage within a decade; foundries holding Arabic type stock watched it become unsellable at home. Some emigrated or turned to decorative work; the domestic trade did not recover.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, arabic_script_calligraphers_and_foundries, payer,
    moderate, biographical, trapped, national).

% Had the lowest schooling access of any group during the campaign years and the highest stake in whatever literacy they already possessed. No consultation mechanism reached them; the evening-course design assumed male, urban, wage-earning availability.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, rural_women_of_transition_generation, excluded,
    powerless, biographical, trapped, national).

% Assess the phonetic-fit claim against other romanization episodes (Uzbek, Azerbaijani, Vietnamese) and against the older abjad's documented performance with Turkish texts. Take no part in Turkish domestic arrangements; publish comparisons that either support or erode the technical rationale.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, international_comparative_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__phonetic_instrumentalism_reading, turkish_state_administration).
narrative_ontology:fixing_cost_class(script_as_identity__phonetic_instrumentalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one shared character encoding for written Turkish: a single alphabet for schooling, printing, administration, signage, and record-keeping, solving the problem of mutually intelligible text production across a national population.
% TRANSFER_FUNCTION: Moves literacy capability between script regimes: devalues Arabic-script human capital held by the Ottoman-trained literate class while conferring Latin-script literacy on the schooled generations; moves the Ottoman Turkish textual archive out of common circulation; concentrates administrative-legibility and symbolic-modernity gains in the state.
% ABSENT_VOICES: Rural women and unschooled adults of the transition generation — the group least reached by evening courses and most dependent on existing literacy — were not consulted. The Arabic-script literate class objected in the 1928 press debates but held no legislative seat after single-party consolidation. Heritage custodians outside the republic (diaspora scholars, Arabist circles) stood entirely outside the decision.
% DISAPPEARANCE_RATIONALE: Every Turkish schoolbook, identity document, court record, newspaper archive, and street sign encodes the standard. Overnight removal would sever the literate population from its own textual environment until a successor encoding was imposed; schooling, publishing, banking, and administration would halt or fork.
% FOUNDING_PROBLEM: Mass illiteracy in the new republic: the 1927 census recorded roughly one person in ten able to read at all, and the Arabic abjad — a consonantal skeleton with sparse vowel marking — fit Turkish's eight-vowel harmonic system poorly, making literacy expensive to acquire and easy for a scribal elite to monopolize.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the republic's own census series (the 1927 baseline against the rises of 1935–1950), contemporaneous foreign-educator assessments of the literacy campaigns, and comparative demography of literacy in non-reforming Persian- and Arabic-script neighbors. The same external sources dispute how much of the gain belongs to script change rather than to the concurrent schooling buildup: the problem's reality and partial resolution are corroborated; its script-specific causation is contested.
narrative_ontology:disappearance_verdict(script_as_identity__phonetic_instrumentalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__phonetic_instrumentalism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__phonetic_instrumentalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__phonetic_instrumentalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__phonetic_instrumentalism_reading, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__phonetic_instrumentalism_reading_tests).
:- end_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.10) because the letterforms price nothing, tax nothing, and gate no revenue: no party collects through the standard's operation. The state's gains are administrative legibility and symbolic capital — real, concentrated, but not rents levied on users. Suppression is authored near-zero (0.06) at interval end because the enforcement machinery has decayed into redundancy; the standard now reproduces itself through schooling and habituation rather than penalty. The high early values in the suppression series record the coercive establishment phase (script bans, compulsory courses, press control), which this reading treats as migration enforcement rather than the arrangement's steady-state character — hence the deliberately traced enforcement-decay curve rather than a flat scalar. Theater ratio 0.22: Alphabet Day commemorations and founder-veneration rhetoric grow as the original justification completes, but the encoding function beneath them remains fully operative, so the rise stays well short of proxy-substitution territory. Accessibility collapse 0.70: within Turkey the alternative — Arabic-script Turkish literacy — has effectively vanished from public life, not because it is impossible but because the state closed it; alternatives persisted elsewhere (diaspora printing, academic Ottomanistics), which holds the value below natural-law levels. Resistance 0.18: sharp conservative and press resistance in 1928–1930 faded with cohort replacement, and no live movement proposes reversal. Claim and metrics are authored independently: the reading claims rope; the metrics describe a matured standard carrying residual transition scars.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the agenda-setter seat the arrangement is a completed public works project: legibility purchased once, enforced briefly, amortized over generations. From the post-reform school seat it is invisible infrastructure — simply how writing is done. From the displaced Ottoman-literate seat the same arrangement reads as confiscation: a lifetime's textual capital devalued by statute, libraries rendered personal dead weight, a professional identity severed. From the 1928-adult seat it is a door closed mid-passage — evening courses offered on a schedule working lives could not meet, after which most simply exited literacy altogether. The engine computes these per-seat classifications from power, exit, and directionality; this reading's rope claim is effectively the agenda-setter's experience generalized to everyone, which is precisely the depoliticizing move the sibling readings contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations — state administration (legibility, conscript literacy, symbolic modernity), post-reform school generations (cheap literacy acquisition), language reformers (professional vindication and institutional position), Latin-print industry (retooling contracts and a captive market) — derive low d: the standard subsidizes these seats. Victim declarations — the Ottoman-script literate class, the 1928 adult population, and the calligraphic/print trade — derive high d, amplified by identity lock for the literate class (exit meant dissolving a constituted scholarly self) and by trap for the adults and tradesmen (script-specific capital, unrecoverable domestically). Because base ε is authored low, nearly all effective extraction concentrates in the trapped and identity-locked payer seats: the per-seat arithmetic is where this reading's 'transition cost' framing meets its limit, since for those seats the cost was total and permanent however small the population-wide average. National spatial scope takes the engine's modest verification-difficulty scaling. No directionality overrides were needed — the derivation from declared roles and exit options captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — script-barrier mass illiteracy — is dead: adult literacy approaches universality, and no plausible recurrence of that problem would restore the arrangement's original justification. Yet the disappearance verdict is world_rearranges and theater remains low: the standard persists because it acquired generic infrastructure value — every text, contract, and database in the republic presupposes it — not because anyone theatrically maintains a corpse. The R5 mismatch consumer will flag dead-status-plus-world-rearranges; the flag resolves here as transformation rather than capture: gain_flow names the state, but the state's accrual is legibility and legitimacy rather than rents extracted through the standard, and fixing_cost is prohibitive in the way a working standard's is — switching costs dwarf any benefit of reversal. The classification guards both mislabelings: it blocks a pure-extraction reading (no collector levies through the letterforms) while the omega variables keep open the charge the sibling readings press — that 'neutral technology' is itself the arrangement's most effective piece of stagecraft, a framing that launders an identity decision as an engineering one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the phonetic_instrumentalism_reading of kernel script_as_identity; what structurally changes under the sibling readings?',
    'Read alongside the sibling files: script_as_identity__kemalist_rupture_reading and script_as_identity__ottoman_continuity_reading author their own ε, beneficiary, and victim structures over the same standing arrangement.',
    'The rupture reading adds civilizational-severance costs this file does not count and raises ε accordingly; the continuity reading recasts the entire Turkish-Muslim readership as victim and the removal of Arabic script as the extraction event itself. Cross-file comparison, not within-file adjustment, is the resolution path.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame placement: one of three readings of the script_as_identity kernel, sharing a referent with reading-indexed ε.').

omega_variable(
    script_neutrality_axiom_status,
    'Does orthographic form carry identity-political content independent of phonetic function — and if so, what does the neutrality premise of this reading fail to count?',
    'Comparative sociolinguistics of script-choice episodes: Uzbek latinization and re-re-latinization, Hindi/Urdu script divergence, Serbo-Croatian digraphia, and the 2014 reintroduction of Ottoman-script electives in Turkish high schools as a live probe of residual identity demand.',
    'If the neutrality axiom fails as a general premise, this reading''s low ε is revealed as framing-dependent: the identity-severance and heritage-access costs omitted here are exactly what the sibling constraints count, and a merged accounting would push the arrangement toward hybrid coordination-with-extraction rather than pure standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_neutrality_axiom_status, conceptual, 'Location of the kernel dispute: neutrality of script form versus constitutive identity content — the axis on which this reading and the continuity reading directly contradict.').

omega_variable(
    literacy_gain_attribution,
    'How much of the measured literacy expansion is attributable to phonetic fit of the Latin alphabet, as opposed to the simultaneous state schooling buildup and the dismantling of the old scribal monopoly?',
    'Counterfactual demographic modeling against non-reforming Persian and Arabic-script neighbors with comparable schooling investment; reanalysis of census microdata separating cohort, region, and schooling exposure.',
    'If schooling dominates, the phonetic-superiority axiom loses its policy force (ε stays low, but the vindicated proposition weakens and the reading''s distinctive claim reduces to timing); if script fit carries a separable effect, the reading''s core empirical claim is strengthened and the rope classification firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_gain_attribution, empirical, 'Attribution ambiguity behind the reading''s central evidentiary claim.').

omega_variable(
    enforcement_decay_vs_cohort_replacement,
    'Did suppression decline because the population came to accept the standard, or because the dissenting cohort simply died out and no successor cohort remembers the alternative?',
    'Cohort-attestation studies comparing attitudes of pre-reform-educated and post-reform-educated generations; persistence of Arabic-script competence and demand in diaspora communities and heritage education.',
    'If decay reflects cohort replacement rather than consent, the low steady-state suppression is latent rather than genuine: revival pressure (as with the Ottoman-script elective courses) could reactivate enforcement needs, and the suppression trajectory should be read as deferred, not resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_vs_cohort_replacement, empirical, 'Whether the enforcement-decay curve records consent or generational attrition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 0, 96).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(scri_tr_t0, observed).
narrative_ontology:measurement(scri_tr_t12, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement_basis(scri_tr_t12, observed).
narrative_ontology:measurement(scri_tr_t24, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement_basis(scri_tr_t24, observed).
narrative_ontology:measurement(scri_tr_t36, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 36, 0.13).
narrative_ontology:measurement_basis(scri_tr_t36, observed).
narrative_ontology:measurement(scri_tr_t48, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 48, 0.15).
narrative_ontology:measurement_basis(scri_tr_t48, observed).
narrative_ontology:measurement(scri_tr_t60, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(scri_tr_t60, observed).
narrative_ontology:measurement(scri_tr_t72, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 72, 0.19).
narrative_ontology:measurement_basis(scri_tr_t72, observed).
narrative_ontology:measurement(scri_tr_t84, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 84, 0.21).
narrative_ontology:measurement_basis(scri_tr_t84, observed).
narrative_ontology:measurement(scri_tr_t96, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 96, 0.22).
narrative_ontology:measurement_basis(scri_tr_t96, observed).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(scri_be_t0, observed).
narrative_ontology:measurement(scri_be_t12, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement_basis(scri_be_t12, observed).
narrative_ontology:measurement(scri_be_t24, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 24, 0.23).
narrative_ontology:measurement_basis(scri_be_t24, observed).
narrative_ontology:measurement(scri_be_t36, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 36, 0.19).
narrative_ontology:measurement_basis(scri_be_t36, observed).
narrative_ontology:measurement(scri_be_t48, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 48, 0.16).
narrative_ontology:measurement_basis(scri_be_t48, observed).
narrative_ontology:measurement(scri_be_t60, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement_basis(scri_be_t60, observed).
narrative_ontology:measurement(scri_be_t72, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 72, 0.12).
narrative_ontology:measurement_basis(scri_be_t72, observed).
narrative_ontology:measurement(scri_be_t84, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 84, 0.11).
narrative_ontology:measurement_basis(scri_be_t84, observed).
narrative_ontology:measurement(scri_be_t96, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 96, 0.1).
narrative_ontology:measurement_basis(scri_be_t96, observed).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(scri_su_t0, observed).
narrative_ontology:measurement(scri_su_t12, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement_basis(scri_su_t12, observed).
narrative_ontology:measurement(scri_su_t24, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 24, 0.37).
narrative_ontology:measurement_basis(scri_su_t24, observed).
narrative_ontology:measurement(scri_su_t36, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 36, 0.29).
narrative_ontology:measurement_basis(scri_su_t36, observed).
narrative_ontology:measurement(scri_su_t48, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 48, 0.22).
narrative_ontology:measurement_basis(scri_su_t48, observed).
narrative_ontology:measurement(scri_su_t60, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 60, 0.16).
narrative_ontology:measurement_basis(scri_su_t60, observed).
narrative_ontology:measurement(scri_su_t72, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 72, 0.12).
narrative_ontology:measurement_basis(scri_su_t72, observed).
narrative_ontology:measurement(scri_su_t84, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 84, 0.08).
narrative_ontology:measurement_basis(scri_su_t84, observed).
narrative_ontology:measurement(scri_su_t96, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 96, 0.06).
narrative_ontology:measurement_basis(scri_su_t96, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, information_standard).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__ottoman_continuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Turkish script reform' covers three structurally distinct assessments of one standing arrangement. Per the ε-invariance principle they are authored as separate stories sharing a referent, linked here: this instrumentalist file (low ε, rope claim), the rupture file (adds civilizational-severance costs to the accounting), and the continuity file (recasts the removal of Arabic script as the extraction event). Upstream/downstream structure: the instrumentalist claim supplied the technical rationale that the rupture narrative cites, so this file influences the rupture sibling; the continuity sibling stands in direct contradiction to this file's neutrality axiom and is marked foreclosed in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
