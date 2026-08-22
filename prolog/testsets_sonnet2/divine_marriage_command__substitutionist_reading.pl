% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Substitutionist Reading: 1890 Manifesto as Superseding Revelation Mandating Monogamy
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This story instantiates the substitutionist reading of the
 *   divine_marriage_command kernel: the 1890 Manifesto is understood by
 *   mainstream church authority as itself a revelation that doctrinally
 *   supersedes and replaces the prior command permitting plural marriage —
 *   not a prudential suspension (continuationist_reading) and not a coerced
 *   institutional accommodation to federal power
 *   (coercion_visibility_reading). Under this reading, continued or resumed
 *   plural marriage after 1890 is not conservative fidelity to an unrescinded
 *   command but apostasy against new, superseding revelation. This framing
 *   does real coordination work — it ends an existential legal conflict,
 *   unifies a fractured membership, and enables mainstream civic
 *   participation — while simultaneously authorizing excommunication of those
 *   who hold the continuationist position. ε is authored for the standing
 *   arrangement (the substitutionist doctrinal-supersession claim and its
 *   enforcement apparatus) as this reading's own lights see it:
 *   moderate-to-high extraction concentrated on those who continued the prior
 *   practice, not zero (as the reading's endorsed alternative — full
 *   doctrinal peace — would imply). A second official 1904 declaration and
 *   subsequent excommunications indicate the substitutionist frame required
 *   active, escalating enforcement to hold in the first two decades before
 *   settling into stable institutional doctrine.
 *
 * KEY AGENTS:
 *   - church_hierarchy: agenda_setter/beneficiary (institutional/analytical) — issues, administers, and enforces the substitutionist frame
 *   - mainstream_membership: beneficiary (organized/constrained) — inherits legal and social normalization
 *   - fundamentalist_polygamist_families: payer (powerless/trapped) — recast from faithful to apostate
 *   - plural_wives_post_manifesto: payer (powerless/trapped) — marital status retroactively delegitimized
 *   - federal_and_state_authorities: excluded (institutional/analytical) — the coercive cause structurally omitted from this reading's account
 *   - historians_and_dissenting_scholars: observer (analytical/analytical) — hold the documentary record of the precipitating coercion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.58).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Substitutionist Reading: 1890 Manifesto as Superseding Revelation Mandating Monogamy").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '2676803e-cb48-44ae-8786-c8c69f6ee4bb').
narrative_ontology:cs_kernel_codification('2676803e-cb48-44ae-8786-c8c69f6ee4bb', formalized).
narrative_ontology:cs_authority_grounding('2676803e-cb48-44ae-8786-c8c69f6ee4bb', lineage).
narrative_ontology:cs_interpretation_layer_present('2676803e-cb48-44ae-8786-c8c69f6ee4bb').
narrative_ontology:cs_reading_relation('2676803e-cb48-44ae-8786-c8c69f6ee4bb', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('2676803e-cb48-44ae-8786-c8c69f6ee4bb', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('2676803e-cb48-44ae-8786-c8c69f6ee4bb', foundational, manifesto_constitutes_superseding_revelation).
narrative_ontology:cs_axiom_status(manifesto_constitutes_superseding_revelation, holdable).
narrative_ontology:cs_axiom_grounding('2676803e-cb48-44ae-8786-c8c69f6ee4bb', manifesto_constitutes_superseding_revelation, theological).
narrative_ontology:cs_axiom('2676803e-cb48-44ae-8786-c8c69f6ee4bb', foundational, prophetic_authority_can_doctrinally_void_prior_command).
narrative_ontology:cs_axiom_status(prophetic_authority_can_doctrinally_void_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('2676803e-cb48-44ae-8786-c8c69f6ee4bb', prophetic_authority_can_doctrinally_void_prior_command, theological).
narrative_ontology:cs_axiom('2676803e-cb48-44ae-8786-c8c69f6ee4bb', secondary, post_manifesto_plural_marriage_constitutes_apostasy).
narrative_ontology:cs_axiom_status(post_manifesto_plural_marriage_constitutes_apostasy, holdable).
narrative_ontology:cs_axiom_grounding('2676803e-cb48-44ae-8786-c8c69f6ee4bb', post_manifesto_plural_marriage_constitutes_apostasy, conventional).
narrative_ontology:cs_reference_frame('2676803e-cb48-44ae-8786-c8c69f6ee4bb', prophetic_continuing_revelation_supremacy).
narrative_ontology:cs_drift_state('2676803e-cb48-44ae-8786-c8c69f6ee4bb', post_second_manifesto_1904_consolidation, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('2676803e-cb48-44ae-8786-c8c69f6ee4bb', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, mainstream_membership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, monogamous_convert_families).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_polygamist_families).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, plural_wives_post_manifesto).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, excommunicated_leaders).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, prophetic_supremacy_over_prior_command).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and administers the Manifesto as binding revelation, sets excommunication policy for post-Manifesto plural marriage, and controls doctrinal messaging. Secured statehood, federal amnesty, and institutional survival by adopting the monogamy standard; now derives legitimacy and mainstream social standing from consistent enforcement of the substitutionist frame.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, church_hierarchy, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__substitutionist_reading, church_hierarchy, beneficiary).

% Accepts the Manifesto as authentic revelation, benefits from reduced social stigma, legal normalization, and full participation in civic and economic life that plural marriage had foreclosed. Exit from this reading would mean re-inheriting the stigma and legal jeopardy the Manifesto resolved.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, mainstream_membership, beneficiary,
    organized, generational, constrained, national).

% Joined or remained in the faith on the understanding that monogamy is the permanent, revealed standard. Their religious commitment is structured entirely around the substitutionist premise; they have no stake in continuationist claims and every incentive to see the prior command as superseded.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, monogamous_convert_families, beneficiary,
    moderate, generational, mobile, national).

% Continue practicing plural marriage on the belief that the original command was never doctrinally rescinded, only suspended. Under this reading they are apostates rather than faithful conservatives; they face excommunication, loss of temple standing, community ostracism, and in some cases legal jeopardy, with no institutional recourse to argue their continuationist premise within mainstream church structures.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamentalist_polygamist_families, payer,
    powerless, generational, trapped, regional).

% Women already sealed in plural marriages before or shortly after 1890 find their marital and inheritance status delegitimized retroactively by the substitutionist framing, without having consented to a doctrinal reversal. Economic dependency and social isolation from mainstream society leave little practical exit.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, plural_wives_post_manifesto, payer,
    powerless, biographical, trapped, local).

% Local leaders and apostles who continued authorizing or entering plural marriages after 1890 were stripped of office and membership under the substitutionist reading's enforcement logic, converting their prior good standing into evidence of apostasy overnight.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, excommunicated_leaders, payer,
    moderate, biographical, constrained, national).

% The coercive pressure (Edmunds-Tucker Act, disincorporation threats, disenfranchisement) that precipitated the Manifesto is structurally present but excluded from the substitutionist narrative, which frames the change as revelation rather than negotiated capitulation. Their role as the proximate cause is not part of this reading's account of legitimacy.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, federal_and_state_authorities, excluded,
    institutional, biographical, analytical, national).

% Document the documentary record of federal pressure, private continuation of plural marriages after 1890, and the gradual hardening of the doctrinal-supersession claim over subsequent decades. Not party to the excommunication apparatus but produce the corroborating record the substitutionist reading must contend with.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, historians_and_dissenting_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution with a single, stable, enforceable marital standard that ends decades of legal conflict with the federal government, unifies membership expectations, and enables full civic participation (statehood, property rights, political enfranchisement) for the mainstream body.
% TRANSFER_FUNCTION: Moves social legitimacy, legal safety, and institutional standing away from fundamentalist and plural-marriage-practicing members and toward the mainstream monogamous body; moves doctrinal authority to interpret and retroactively characterize prior revelation from local/plural-marriage leadership to the centralized hierarchy.
% ABSENT_VOICES: Fundamentalist polygamist families and the plural wives married before or shortly after 1890 would object that the doctrine was never rescinded, only suspended under duress, and that they are being punished for continuity rather than departure — but they hold no seat within mainstream church courts or doctrinal bodies where the substitutionist framing is adjudicated.
% DISAPPEARANCE_RATIONALE: If the substitutionist reading were abandoned in favor of the continuationist reading, excommunicated fundamentalist communities would be doctrinally rehabilitated, current temple worthiness and membership boundaries would be redrawn, and the church's century of civic normalization built on the monogamy standard would be destabilized — mainstream membership, legal status, and institutional relationships with the state would all require renegotiation.
% FOUNDING_PROBLEM: The church faced federal disincorporation, mass disenfranchisement, and confiscation of temple property under anti-polygamy statutes; the Manifesto was issued to resolve an existential institutional crisis by ending the practice the government was suppressing.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the church hierarchy (documenting the Edmunds-Tucker Act enforcement, the timing of the 1890 declaration relative to federal legal deadlines, and private continuation of plural marriages by church leaders for over a decade after the Manifesto) attest that the precipitating federal-coercion crisis is long resolved; the church hierarchy itself, as the benefiting party, attests instead that the founding problem was never coercion but the arrival of superseding revelation — the substitutionist account is corroborated only from within the benefiting institution.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects substantial but not maximal extraction: the substitutionist frame concentrates real costs (loss of standing, marital delegitimization, excommunication) on a bounded population while providing genuine coordination benefits to the much larger mainstream body. Suppression (0.72) is high and structural — the frame is maintained through excommunication courts, doctrinal gatekeeping, and social ostracism of dissenters, not merely voluntary consensus. Theater ratio (0.44) is elevated because a meaningful share of the doctrinal apparatus (annual restatements, official declarations, disavowals) functions to perform the supersession narrative rather than resolve any remaining live practice, particularly after fundamentalist splinter groups had already separated institutionally by the mid-20th century. Accessibility collapse (0.62) is moderate-high: once the substitutionist frame is institutionally adopted, alternative readings become structurally unavailable within mainstream church courts, though they persist outside the institution among fundamentalist groups — collapse is real but not total. Resistance (0.55) is substantial and organized (fundamentalist communities maintain their own religious and legal institutions contesting the substitutionist claim), which is why this is authored as tangled_rope rather than snare: there is a genuine coordination function (ending a genuinely existential institutional crisis) alongside genuine, asymmetric extraction from an identifiable victim population.
 *
 * DIRECTIONALITY LOGIC:
 *   Church hierarchy sits at the full-beneficiary end: it authored the reframing, administers enforcement, and derives its post-1890 institutional survival and civic legitimacy from the substitutionist claim. Mainstream membership and monogamous convert families are structural beneficiaries with moderate-to-mobile exit — their religious identity does not depend on the continuationist premise being true. Fundamentalist polygamist families, plural wives married under the prior command, and excommunicated leaders are the targets: trapped or constrained exit, generational time horizon, and the doctrinal shift converts their prior good standing into grounds for expulsion without their having changed behavior or belief. Federal and state authorities are excluded from the reading's own narrative account even though they are the proximate structural cause of the shift — this is the coercion_visibility_reading's subject, not this one's.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — federal legal existential threat to the institution — is dead by outside corroboration (legal historians, the documentary record of Edmunds-Tucker enforcement) but the substitutionist doctrinal-supersession claim persists and has hardened rather than relaxed, continuing to authorize excommunication over a century after the precipitating crisis resolved. This is the mismatch the R5 interview is built to surface: founding_problem_status = dead, disappearance_verdict = world_rearranges. That combination is a candidate zombie-mandate flag — the coordination function that justified the original transition (ending federal conflict) is long satisfied, yet the enforcement apparatus built on the substitutionist reading continues to reorganize lives (excommunication, marital delegitimization) as if the crisis were still live. Classifying this as tangled_rope rather than snare preserves the fact that the frame did solve a genuine, severe coordination problem in 1890 — the type does not erase that history — while still registering the asymmetric, ongoing extraction from fundamentalist and plural-marriage-affected populations as real and structurally continuous with, not incidental to, the doctrinal move.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supersession_vs_suspension_ambiguity,
    'Did the 1890 Manifesto doctrinally rescind the prior command permitting plural marriage, or merely suspend its practice under external duress while leaving the underlying revelation theologically intact?',
    'Comparative textual analysis of the Manifesto''s own language against subsequent official declarations (1904) and private correspondence of church leadership from the period; cross-reference against later official statements characterizing the Manifesto''s doctrinal status.',
    'If suspension rather than supersession is the better-supported reading, the substitutionist reading''s excommunication apparatus loses its doctrinal warrant and the continuationist_reading''s victim set (currently treated as apostate) would be recharacterized as doctrinally faithful — this is the live contest between this story and continuationist_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_vs_suspension_ambiguity, conceptual, 'Whether the Manifesto is best read as doctrinal rescission or prudential suspension.').

omega_variable(
    revelation_vs_coercion_legitimacy_grounding,
    'Does the substitutionist reading''s legitimacy rest genuinely on revelatory content, or is the revelation framing itself a retroactive legitimation of a decision forced by federal coercion?',
    'Historical analysis of the timing and content of the Manifesto relative to federal legal deadlines and property confiscation threats, weighed against the internal theological account of how the revelation was received.',
    'If coercion is the better-supported grounding, this story''s ε and stakeholder set are unaffected (ε is authored for the standing arrangement under this reading''s own lights per the fixed-referent rule) but the reading''s own claim to legitimacy is undermined relative to the coercion_visibility_reading, which takes coercion as the explicit legitimacy ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_coercion_legitimacy_grounding, conceptual, 'Whether revelatory or coercive legitimation better explains the Manifesto''s adoption and persistence.').

omega_variable(
    excommunication_proportionality_ambiguity,
    'Is the continued excommunication of fundamentalist practitioners over a century after the precipitating federal crisis proportionate to any live institutional threat, or is it inertial enforcement of a doctrinal boundary whose original justification has lapsed?',
    'Compare enforcement intensity (excommunication rates, doctrinal restatement frequency) against independent measures of any remaining legal or civic threat to the institution across the interval.',
    'If enforcement is disproportionate to any live threat, this supports the mandatrophy flag (founding_problem_status=dead, disappearance_verdict=world_rearranges) and would push the classification toward recognizing a growing snare-like component within the tangled_rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excommunication_proportionality_ambiguity, empirical, 'Whether ongoing excommunication enforcement is proportionate to any remaining institutional threat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 1890, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__substitutionist_reading, theater_ratio, 1890, 0.75).
narrative_ontology:measurement(divi_tr_t1904, divine_marriage_command__substitutionist_reading, theater_ratio, 1904, 0.6).
narrative_ontology:measurement(divi_tr_t1930, divine_marriage_command__substitutionist_reading, theater_ratio, 1930, 0.5).
narrative_ontology:measurement(divi_tr_t1960, divine_marriage_command__substitutionist_reading, theater_ratio, 1960, 0.42).
narrative_ontology:measurement(divi_tr_t1990, divine_marriage_command__substitutionist_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(divi_tr_t2020, divine_marriage_command__substitutionist_reading, theater_ratio, 2020, 0.44).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__substitutionist_reading, base_extractiveness, 1890, 0.68).
narrative_ontology:measurement(divi_be_t1904, divine_marriage_command__substitutionist_reading, base_extractiveness, 1904, 0.66).
narrative_ontology:measurement(divi_be_t1930, divine_marriage_command__substitutionist_reading, base_extractiveness, 1930, 0.6).
narrative_ontology:measurement(divi_be_t1960, divine_marriage_command__substitutionist_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(divi_be_t1990, divine_marriage_command__substitutionist_reading, base_extractiveness, 1990, 0.56).
narrative_ontology:measurement(divi_be_t2020, divine_marriage_command__substitutionist_reading, base_extractiveness, 2020, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__substitutionist_reading, suppression_requirement, 1890, 0.85).
narrative_ontology:measurement(divi_su_t1904, divine_marriage_command__substitutionist_reading, suppression_requirement, 1904, 0.9).
narrative_ontology:measurement(divi_su_t1930, divine_marriage_command__substitutionist_reading, suppression_requirement, 1930, 0.78).
narrative_ontology:measurement(divi_su_t1960, divine_marriage_command__substitutionist_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(divi_su_t1990, divine_marriage_command__substitutionist_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(divi_su_t2020, divine_marriage_command__substitutionist_reading, suppression_requirement, 2020, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three stories decomposing the divine_marriage_command kernel per the ε-invariance principle. continuationist_reading authors the same historical episode with the prior command treated as theologically unrescinded (different victim set: mainstream excommunicating authority becomes the extractive party against a faithful-continuation population). coercion_visibility_reading authors legitimacy as grounded in institutional-survival necessity under federal coercion rather than revelatory content, backgrounding the excommunication apparatus and foregrounding the federal_and_state_authorities seat this story excludes. All three share the kernel (what the Manifesto doctrinally means and does) but diverge in ε referent framing, beneficiary/victim structure, and which axioms are held foundational versus foreclosed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
