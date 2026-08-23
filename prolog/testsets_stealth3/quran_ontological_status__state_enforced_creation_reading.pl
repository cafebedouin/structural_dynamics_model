% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ABOLISHED — enforcement rescinded 848-851 CE]
% ============================================================================

:- module(constraint_quran_ontological_status__state_enforced_creation_reading, []).

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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: The Mihna: Caliphal Enforcement of the Created-Qur'an Doctrine
 *   domain: religious-political/theological
 *
 * SUMMARY:
 *   Between 833 and 851 the Abbasid state compelled judges, jurists, and
 *   tradition-transmitters to publicly affirm the Mu'tazilite thesis that the
 *   Qur'an is created, examining them in tribunals and punishing refusal with
 *   removal, imprisonment, flogging, and exile. The campaign — the mihna —
 *   converted a live metaphysical dispute into an instrument for
 *   redistributing religious office and staging the caliph as arbiter of
 *   doctrine; it collapsed when a new caliph switched patronage to the
 *   refusers' side. This file instantiates ONE reading of the kernel
 *   quran_ontological_status (the state_enforced_creation_reading) as a clean
 *   epsilon-invariant constraint: the referent of extractiveness is the
 *   standing enforcement arrangement itself, priced by this reading's own
 *   lights, not the doctrinal thesis in isolation (that is the sibling story)
 *   and not the arrangement the refusers would have preferred. KEY AGENTS (by
 *   structural relationship): the palace that issued and later revoked the
 *   tests; the court theologians whose school supplied doctrine and absorbed
 *   the vacated offices; the refusing scholars anchored in popular devotion;
 *   the signatory jurists who bought safety with public recantation; the
 *   examiner corps that ran the panels; the unseated lay public whose
 *   devotional life the imposed teaching touched; and the annalists who
 *   preserved both ledgers.
 *
 * KEY AGENTS:
 *   - abbasid_caliphate: Agenda-setting sovereign (institutional/arbitrage) — issues the creed test, appoints examiners, collects the authority dividend, and reverses the whole arrangement by decree fifteen years in
 *   - mu_tazilite_court_theologians: Doctrine-supplying officeholder caste (institutional/constrained) — gains chairs, stipends, and standing while the policy lasts; loses all of it when the palace flips
 *   - traditionalist_scholars: Principal refusing class (moderate/constrained) — summoned to sign, imprisoned and flogged for refusal, protected by popular veneration
 *   - compliant_jurists: Dual-positioned signatories (moderate/constrained) — purchase safety and office with a public sentence they privately disbelieve
 *   - mihna_tribunal_examiners: Enforcement corps (organized/mobile) — administer the examinations; advance with success, fall under investigation after the reversal
 *   - devotional_lay_public: Unseated cost-bearing public (organized/trapped) — absorbs the imposed teaching with no deliberative place; expresses dissent only as crowds
 *   - contemporary_annalists: Analytical observer (analytical/analytical) — preserves tribunal records and prison accounts from outside both camps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.79).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.72).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "The Mihna: Caliphal Enforcement of the Created-Qur'an Doctrine").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "religious-political/theological").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '7c708d6d-4e6f-4519-b20a-7e9e2432638a').
narrative_ontology:cs_kernel_codification('7c708d6d-4e6f-4519-b20a-7e9e2432638a', formalized).
narrative_ontology:cs_authority_grounding('7c708d6d-4e6f-4519-b20a-7e9e2432638a', extraction).
narrative_ontology:cs_interpretation_layer_present('7c708d6d-4e6f-4519-b20a-7e9e2432638a').
narrative_ontology:cs_reading_relation('7c708d6d-4e6f-4519-b20a-7e9e2432638a', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('7c708d6d-4e6f-4519-b20a-7e9e2432638a', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('7c708d6d-4e6f-4519-b20a-7e9e2432638a', foundational, caliphal_creedsupremacy_enforcement).
narrative_ontology:cs_axiom_status(caliphal_creedsupremacy_enforcement, overridden).
narrative_ontology:cs_axiom_grounding('7c708d6d-4e6f-4519-b20a-7e9e2432638a', caliphal_creedsupremacy_enforcement, conventional).
narrative_ontology:cs_axiom('7c708d6d-4e6f-4519-b20a-7e9e2432638a', foundational, created_quran_binding_orthodoxy).
narrative_ontology:cs_axiom_status(created_quran_binding_orthodoxy, holdable).
narrative_ontology:cs_axiom_grounding('7c708d6d-4e6f-4519-b20a-7e9e2432638a', created_quran_binding_orthodoxy, deontological).
narrative_ontology:cs_reference_frame('7c708d6d-4e6f-4519-b20a-7e9e2432638a', imamate_defined_orthodoxy).
narrative_ontology:cs_drift_state('7c708d6d-4e6f-4519-b20a-7e9e2432638a', mutawakkil_reversal_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7c708d6d-4e6f-4519-b20a-7e9e2432638a', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_court_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, devotional_lay_public).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, compliant_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, compliant_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mihna_tribunal_examiners).
narrative_ontology:constraint_vindicates(quran_ontological_status__state_enforced_creation_reading, caliphal_creedsupremacy_prerogative).
narrative_ontology:constraint_vindicates(quran_ontological_status__state_enforced_creation_reading, created_quran_metaphysics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ruled an empire whose judges, teachers, and report-transmitters answered increasingly to their own scholarly networks rather than the palace. In 833 the throne ordered holders of religious office to publicly affirm that the Qur'an is created, appointed examiners to put the question, and set removal, prison, and the lash as the consequences of refusal. The campaign let the palace decide who held religious employment and staged the ruler as final arbiter over the community's most sensitive question. Fifteen years later a new caliph cancelled the tests, released the prisoners, and shifted patronage to the refusers' side; the palace could reverse course at will, having risked nothing structural in either direction.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate, agenda_setter,
    institutional, generational, arbitrage, continental).

% Staffed the argumentative side of the campaign: their masters supplied the reasoning the examiners tested for, and their members filled judgeships and teaching posts vacated by the purged. The school gained official standing, stipends, and students on a scale it had never known. Every advantage flowed through continued royal favor; when the palace changed sides, the same doors closed, and adherents spent the following decades defending themselves against charges of having served the persecuting party.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_court_theologians, beneficiary,
    institutional, biographical, constrained, continental).

% Preserved and transmitted reports of the prophet's practice and the devotional life built upon them, teaching in Baghdad's mosques and private circles. Summoned before examining panels and asked to sign a declaration that the Qur'an is created, the leading figure refused, was chained in a Baghdad prison for roughly two years and flogged until he collapsed, and still declined the signature offered as the price of release. Others fled to distant garrison towns, accepted prison, or signed under duress and carried lifelong reproach for it. Refusal cost them freedom and health; their compensation, deferred, was the reverence of the city crowds that gathered wherever they appeared.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    moderate, generational, constrained, continental).

% Signed when the panel placed the declaration before them, keeping their posts, incomes, and liberty. The signature purchased safety at the price of disavowing, in front of colleagues, teachings many continued to hold privately; biographical dictionaries record several spending their later years visiting the famous prisoner to ask how refusal could be borne, and carrying a lasting stain in learned company. What they kept was continuity of livelihood; what they paid was public ownership of a sentence they did not believe.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, compliant_jurists, payer,
    moderate, immediate, constrained, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, compliant_jurists, beneficiary).

% Carried out the campaign in the capital and the provincial centers: drawing up lists of report-transmitters and judges, summoning them, administering the affirmation formula, recording signatures, and forwarding refusers to prison. Promotion followed successful administration; hesitancy invited replacement. When the order came down to stop, they stopped, and several of the most zealous spent the following years under investigation themselves.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mihna_tribunal_examiners, agenda_setter,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, mihna_tribunal_examiners, beneficiary).

% Recited the book five times daily, memorized it, wept over it, and taught it to their children. Official preaching told them the text they held sacred was a produced thing, and the same authorities expected them to repeat that teaching when inspected. They had no seat on any panel and no petition channel; their registered dissent took the form of crowds at the prisoner's door and occasional street unrest in Baghdad, and after the reversal their allegiance settled decisively on the refusers' side.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, devotional_lay_public, payer,
    organized, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, devotional_lay_public, excluded).

% A generation later compiled reign-by-reign histories from eyewitness reports, palace correspondence, and the biographical notices kept by scholars' own students, preserving the text of the original order, the names of signatories and refusers, and the account of the prisoner's trial. Standing outside both camps' institutional interests, they are the nearest thing this episode has to a ledger keeper.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, contemporary_annalists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized a single official answer to the community's most divisive doctrinal question across the empire's courts, mosques, and teaching posts, replacing a fragmented landscape of competing schools with one state-certified formula — solved centrally, by examination, rather than through scholarly consensus.
% TRANSFER_FUNCTION: Moved public doctrinal assent from the scholarly class to the throne: signatories surrendered epistemic independence and received offices, salaries, and safety in return; refusers paid with posts, liberty, and skin, transferring their standing involuntarily to the compliant and to the palace that ranked them.
% ABSENT_VOICES: The panels summoned named individuals from the scholarly elite, so consent was manufactured across a handpicked sample: rank-and-file reciters, popular preachers, women who taught the book in homes, and non-elite devotees — the people whose daily practice the disputed doctrine touched most intimately — were never asked and had no channel to object. Future generations bound by whichever settlement emerged were absent by construction. Their interests surface in the record only as crowd sentiment and, later, as the verdict of the biographical literature.
% DISAPPEARANCE_RATIONALE: Had the tests, the prison terms, and the patronage machine vanished at their height, dismissed judges would have returned to their benches, exiles home, the palace's claim to define doctrine would have collapsed overnight, and Baghdad's teaching circles would have reorganized around the refusers' moral victory — substantially what happened when abolition came for real in 848-851.
% FOUNDING_PROBLEM: After a civil war and a contested succession, al-Ma'mun ruled a state whose religious class had grown wealthy, popular, and semi-independent, while the dynasty needed a visible instrument of religious primacy. His tutors' rationalist theology supplied a sharp, binary test — is the Qur'an created? — whose answers sorted the scholarly class into compliant officeholders and refusers, letting the crown redistribute religious employment and stage the ruler as guardian of pure doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Court-external attestation exists for the events and the targeting pattern: the annalistic tradition (al-Tabari, compiling eyewitness reports), the biographical dictionaries of the refusing scholars written by their own students, and surviving correspondence. What no source outside the benefiting parties attests is that the founding problem — durable caliphal custody of doctrine — was tractable by this instrument; the palace's own abandonment within fifteen years is the strongest external evidence that it was not. Parties dispute whether the underlying throne-versus-scholarship tension remained live: later caliphs kept issuing creeds and persecuting rivals, while the scholarly class consolidated the autonomy this campaign failed to break.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.79, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__state_enforced_creation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__state_enforced_creation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores characterize the arrangement at operative maturity (the 839-845 plateau), not its terminal state: extractiveness 0.79 reflects the full price schedule (office loss, imprisonment, flogging, exile) levied for doctrinal nonconformity; suppression 0.72 is structural — tribunals, prisons, patronage denial — with a minor internalized residue (the signatories' self-reproach recorded in the biographical literature) noted but not driving the number; theater_ratio 0.32 because the examination ritual performed impartial doctrinal scrutiny while functioning as an office-purge filter. Accessibility_collapse 0.62: alternatives (flight, dissimulation, nominal signature) survived, but open doctrinal alternatives did not. Resistance 0.68 is unusually high for an enforcement regime and is part of what killed it. Coalition potential: the lay public's crowd capacity was the one coalition lever the paying side held, and it mattered — visible veneration of the prisoner raised the campaign's cost and narrowed the palace's options; no paying seat combined coalition capacity with a deliberative channel, which is why the arrangement lasted as long as it did. The measurement series run on one shared seven-point grid (833-851, all three metrics at every point). Because this lifecycle is rise-and-abolition rather than monotone accumulation, the terminal grid snapshot (851) diverges from the plateau-valued base scores by design — that divergence is authored deliberately and documented here rather than reconciled by flattening either surface. The collapse is externally driven (succession politics flipped palace incentives), not internal atrophy, so no cyclical pattern is claimed. identity_coordination is declared because the arrangement's coordination claim is boundary-maintenance (who counts as orthodox); the FNL gaming caveat applies squarely — much of what the claim packages as identity coordination is enforced membership, and the Boltzmann coupling test is expected to flag it.
 *
 * PERSPECTIVAL GAP:
 *   The palace seat computes an instrument it wielded and cheaply discarded — its own reversal is the strongest anti-naturalness datum in the record. The refusing scholars' seat computes persecution met with witness. The compliant jurists' seat computes a recurring tax paid in public speech. The examiners' seat computes career machinery. Same arrangement, four experienced types; the engine derives each from power, exit, and declared position, and the gap between the palace's self-experience and the payers' experience is precisely what the per-seat computation exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared structure maps cleanly onto the derivation chain, so no overrides are authored: the palace (agenda_setter plus beneficiary, arbitrage exit) sits nearest the beneficiary pole; the court theologians (beneficiary, constrained exit — their entire position rented from royal favor) sit low but not at zero, since their standing was contingent and reversible; the refusing scholars and the lay public (payers, constrained/trapped) sit near the target pole, the trapped lay public pushed furthest; the compliant jurists (payer plus beneficiary) net out between the refusers and the no-signature counterfactual; the examiners (agenda_setter plus beneficiary, mobile) sit low. Continental scope amplifies effective extraction on the target seats: verifying conformity across an empire's pulpits and classrooms is expensive, and the tribunal machinery existed precisely because conformity could not verify itself. Suppression enters the arithmetic unscaled, as a raw structural property; only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification blocks two symmetrical misreadings. Reading the arrangement as rope or scaffold accepts the palace's uniformity story: but the examination targeted refusal, not fragmentation, and no sunset clause was ever contemplated — it claimed permanence and died by palace politics instead of transitioning out. Reading its rapid collapse as piton atrophy mistakes the cause: it was abolished while functionally intact, so theater_ratio never entered the hollow-shell range and no inertial remnant persisted — the mandate and the arrangement died together, which is why no mandatrophy resolution is declared. The R5 interview is authored with founding_problem_status 'contested' rather than 'dead': a dead-status authorship would trip the capture/zombie mismatch flag against a computed piton path that never existed, since the world demonstrably rearranged on schedule; the underlying throne-versus-scholarship tension persisted in mutated form under later caliphs, which is exactly what the contested status encodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'This story is one reading of kernel quran_ontological_status (reading: state_enforced_creation_reading). Is the measured extraction a property of enforcing any metaphysical claim by state power, or specific to the created-Qur''an content — and how would the sibling readings (uncreated_reading, created_reading) restructure the beneficiary/victim surface?',
    'Read the three sibling stories side-by-side: if created_reading (pure doctrine, no enforcement) authors materially lower epsilon while this story prices tribunals and purges, the extraction lives in the enforcement layer rather than the theology; the uncreated_reading sibling should show the refusing party''s costs from the opposite seat.',
    'If enforcement-generic, the family''s classification variance is political rather than theological, and any doctrine paired with this machinery would classify alike; if content-specific, the doctrinal premise itself carries part of the measured burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Where the disagreement between kernel readings is located: enforcement layer versus doctrinal content.').

omega_variable(
    conviction_versus_instrument,
    'Was the campaign primarily al-Ma''mun''s personal rationalist conviction or a bureaucratic-political instrument continued opportunistically by successors with less investment in the doctrine?',
    'Philology of the 833 edict letter and comparison of successor behavior: al-Mu''tasim prosecuted the famous refuser with personal attention; al-Wathiq industrialized the exile pipeline; al-Mutawakkil cancelled the whole apparatus within a year of accession.',
    'A conviction-heavy reading strengthens the coordination component (tangled_rope shading: genuine doctrinal program plus extraction); an instrument-heavy reading confirms the snare — doctrine as pretext, office-redistribution as function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conviction_versus_instrument, empirical, 'Motivational composition of the enforcement campaign.').

omega_variable(
    consent_coercion_composition,
    'What share of elite assent to the enforced formula was coerced rather than persuaded?',
    'Compare signing rates across provinces differing in tribunal intensity, using the biographical registers of who signed where and under what recorded pressure.',
    'A higher coerced share attributes more of the doctrine''s apparent spread to the suppression machinery rather than to the argument, raising the effective weight of the enforcement layer in any downstream assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_coercion_composition, empirical, 'Composition of doctrinal assent under examination conditions.').

omega_variable(
    hagiographic_resistance_bias,
    'Does the resistance measurement inherit survivorship inflation from victor-written martyrdom literature, given that the refusers'' side ultimately won the orthodoxy settlement?',
    'Triangulate the biographical notices with Mu''tazilite-side accounts and administrative notices of actual compliance rates; discount celebrated refusal by the ratio of signatories to refusers in the preserved lists.',
    'Corrected resistance may drop by roughly a tenth; the classification is robust to the correction since a snare tolerates moderate resistance — but the drift-detection trajectory near the collapse would flatten.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hagiographic_resistance_bias, empirical, 'Source bias in the resistance record.').

omega_variable(
    grid_endpoint_snapshot_uncertainty,
    'The coercion-grid endpoints are level-resolved judgments drawn from narrative sources, not measurements — how firm are the class-level and individual-level values at 833 and 851?',
    'Prosopographic sampling of scholarly careers across the interval: entry, summons, response, and outcome by province and decade, converting anecdote into rates.',
    'Firmer endpoints would sharpen the gradient/kappa track outputs; until then the authored values are conservative judgments and the tracks should be read as provisional rather than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_endpoint_snapshot_uncertainty, empirical, 'Uncertainty attaching to the authored coercion-grid endpoint values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 833, 851).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mihna_quran_created_tr_t833, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 833, 0.22).
narrative_ontology:measurement_basis(mihna_quran_created_tr_t833, observed).
narrative_ontology:measurement(mihna_quran_created_tr_t836, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 836, 0.26).
narrative_ontology:measurement_basis(mihna_quran_created_tr_t836, observed).
narrative_ontology:measurement(mihna_quran_created_tr_t839, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 839, 0.3).
narrative_ontology:measurement_basis(mihna_quran_created_tr_t839, observed).
narrative_ontology:measurement(mihna_quran_created_tr_t842, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 842, 0.35).
narrative_ontology:measurement_basis(mihna_quran_created_tr_t842, observed).
narrative_ontology:measurement(mihna_quran_created_tr_t845, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 845, 0.37).
narrative_ontology:measurement_basis(mihna_quran_created_tr_t845, observed).
narrative_ontology:measurement(mihna_quran_created_tr_t848, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 848, 0.29).
narrative_ontology:measurement_basis(mihna_quran_created_tr_t848, observed).
narrative_ontology:measurement(mihna_quran_created_tr_t851, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 851, 0.23).
narrative_ontology:measurement_basis(mihna_quran_created_tr_t851, observed).

% Extraction over time
narrative_ontology:measurement(mihna_quran_created_be_t833, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 833, 0.55).
narrative_ontology:measurement_basis(mihna_quran_created_be_t833, observed).
narrative_ontology:measurement(mihna_quran_created_be_t836, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 836, 0.68).
narrative_ontology:measurement_basis(mihna_quran_created_be_t836, observed).
narrative_ontology:measurement(mihna_quran_created_be_t839, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 839, 0.74).
narrative_ontology:measurement_basis(mihna_quran_created_be_t839, observed).
narrative_ontology:measurement(mihna_quran_created_be_t842, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 842, 0.8).
narrative_ontology:measurement_basis(mihna_quran_created_be_t842, observed).
narrative_ontology:measurement(mihna_quran_created_be_t845, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 845, 0.77).
narrative_ontology:measurement_basis(mihna_quran_created_be_t845, observed).
narrative_ontology:measurement(mihna_quran_created_be_t848, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 848, 0.44).
narrative_ontology:measurement_basis(mihna_quran_created_be_t848, observed).
narrative_ontology:measurement(mihna_quran_created_be_t851, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 851, 0.21).
narrative_ontology:measurement_basis(mihna_quran_created_be_t851, observed).

% Suppression requirement over time
narrative_ontology:measurement(mihna_quran_created_su_t833, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 833, 0.5).
narrative_ontology:measurement_basis(mihna_quran_created_su_t833, observed).
narrative_ontology:measurement(mihna_quran_created_su_t836, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 836, 0.63).
narrative_ontology:measurement_basis(mihna_quran_created_su_t836, observed).
narrative_ontology:measurement(mihna_quran_created_su_t839, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 839, 0.71).
narrative_ontology:measurement_basis(mihna_quran_created_su_t839, observed).
narrative_ontology:measurement(mihna_quran_created_su_t842, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 842, 0.76).
narrative_ontology:measurement_basis(mihna_quran_created_su_t842, observed).
narrative_ontology:measurement(mihna_quran_created_su_t845, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 845, 0.73).
narrative_ontology:measurement_basis(mihna_quran_created_su_t845, observed).
narrative_ontology:measurement(mihna_quran_created_su_t848, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 848, 0.38).
narrative_ontology:measurement_basis(mihna_quran_created_su_t848, observed).
narrative_ontology:measurement(mihna_quran_created_su_t851, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 851, 0.17).
narrative_ontology:measurement_basis(mihna_quran_created_su_t851, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=833, tn=851
narrative_ontology:measurement(mihna_quran_created_grid_01, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(class), 833, 0.35).
narrative_ontology:measurement_basis(mihna_quran_created_grid_01, observed).
narrative_ontology:measurement(mihna_quran_created_grid_02, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(class), 851, 0.25).
narrative_ontology:measurement_basis(mihna_quran_created_grid_02, observed).
narrative_ontology:measurement(mihna_quran_created_grid_03, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(individual), 833, 0.3).
narrative_ontology:measurement_basis(mihna_quran_created_grid_03, observed).
narrative_ontology:measurement(mihna_quran_created_grid_04, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(individual), 851, 0.22).
narrative_ontology:measurement_basis(mihna_quran_created_grid_04, observed).
narrative_ontology:measurement(mihna_quran_created_grid_05, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(organizational), 833, 0.5).
narrative_ontology:measurement_basis(mihna_quran_created_grid_05, observed).
narrative_ontology:measurement(mihna_quran_created_grid_06, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(organizational), 851, 0.2).
narrative_ontology:measurement_basis(mihna_quran_created_grid_06, observed).
narrative_ontology:measurement(mihna_quran_created_grid_07, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(structural), 833, 0.45).
narrative_ontology:measurement_basis(mihna_quran_created_grid_07, observed).
narrative_ontology:measurement(mihna_quran_created_grid_08, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(structural), 851, 0.18).
narrative_ontology:measurement_basis(mihna_quran_created_grid_08, observed).
narrative_ontology:measurement(mihna_quran_created_grid_09, quran_ontological_status__state_enforced_creation_reading, resistance(class), 833, 0.3).
narrative_ontology:measurement_basis(mihna_quran_created_grid_09, observed).
narrative_ontology:measurement(mihna_quran_created_grid_10, quran_ontological_status__state_enforced_creation_reading, resistance(class), 851, 0.35).
narrative_ontology:measurement_basis(mihna_quran_created_grid_10, observed).
narrative_ontology:measurement(mihna_quran_created_grid_11, quran_ontological_status__state_enforced_creation_reading, resistance(individual), 833, 0.35).
narrative_ontology:measurement_basis(mihna_quran_created_grid_11, observed).
narrative_ontology:measurement(mihna_quran_created_grid_12, quran_ontological_status__state_enforced_creation_reading, resistance(individual), 851, 0.3).
narrative_ontology:measurement_basis(mihna_quran_created_grid_12, observed).
narrative_ontology:measurement(mihna_quran_created_grid_13, quran_ontological_status__state_enforced_creation_reading, resistance(organizational), 833, 0.2).
narrative_ontology:measurement_basis(mihna_quran_created_grid_13, observed).
narrative_ontology:measurement(mihna_quran_created_grid_14, quran_ontological_status__state_enforced_creation_reading, resistance(organizational), 851, 0.25).
narrative_ontology:measurement_basis(mihna_quran_created_grid_14, observed).
narrative_ontology:measurement(mihna_quran_created_grid_15, quran_ontological_status__state_enforced_creation_reading, resistance(structural), 833, 0.15).
narrative_ontology:measurement_basis(mihna_quran_created_grid_15, observed).
narrative_ontology:measurement(mihna_quran_created_grid_16, quran_ontological_status__state_enforced_creation_reading, resistance(structural), 851, 0.3).
narrative_ontology:measurement_basis(mihna_quran_created_grid_16, observed).
narrative_ontology:measurement(mihna_quran_created_grid_17, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(class), 833, 0.4).
narrative_ontology:measurement_basis(mihna_quran_created_grid_17, observed).
narrative_ontology:measurement(mihna_quran_created_grid_18, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(class), 851, 0.15).
narrative_ontology:measurement_basis(mihna_quran_created_grid_18, observed).
narrative_ontology:measurement(mihna_quran_created_grid_19, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(individual), 833, 0.5).
narrative_ontology:measurement_basis(mihna_quran_created_grid_19, observed).
narrative_ontology:measurement(mihna_quran_created_grid_20, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(individual), 851, 0.15).
narrative_ontology:measurement_basis(mihna_quran_created_grid_20, observed).
narrative_ontology:measurement(mihna_quran_created_grid_21, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(organizational), 833, 0.55).
narrative_ontology:measurement_basis(mihna_quran_created_grid_21, observed).
narrative_ontology:measurement(mihna_quran_created_grid_22, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(organizational), 851, 0.2).
narrative_ontology:measurement_basis(mihna_quran_created_grid_22, observed).
narrative_ontology:measurement(mihna_quran_created_grid_23, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(structural), 833, 0.6).
narrative_ontology:measurement_basis(mihna_quran_created_grid_23, observed).
narrative_ontology:measurement(mihna_quran_created_grid_24, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(structural), 851, 0.25).
narrative_ontology:measurement_basis(mihna_quran_created_grid_24, observed).
narrative_ontology:measurement(mihna_quran_created_grid_25, quran_ontological_status__state_enforced_creation_reading, suppression(class), 833, 0.3).
narrative_ontology:measurement_basis(mihna_quran_created_grid_25, observed).
narrative_ontology:measurement(mihna_quran_created_grid_26, quran_ontological_status__state_enforced_creation_reading, suppression(class), 851, 0.1).
narrative_ontology:measurement_basis(mihna_quran_created_grid_26, observed).
narrative_ontology:measurement(mihna_quran_created_grid_27, quran_ontological_status__state_enforced_creation_reading, suppression(individual), 833, 0.28).
narrative_ontology:measurement_basis(mihna_quran_created_grid_27, observed).
narrative_ontology:measurement(mihna_quran_created_grid_28, quran_ontological_status__state_enforced_creation_reading, suppression(individual), 851, 0.1).
narrative_ontology:measurement_basis(mihna_quran_created_grid_28, observed).
narrative_ontology:measurement(mihna_quran_created_grid_29, quran_ontological_status__state_enforced_creation_reading, suppression(organizational), 833, 0.4).
narrative_ontology:measurement_basis(mihna_quran_created_grid_29, observed).
narrative_ontology:measurement(mihna_quran_created_grid_30, quran_ontological_status__state_enforced_creation_reading, suppression(organizational), 851, 0.15).
narrative_ontology:measurement_basis(mihna_quran_created_grid_30, observed).
narrative_ontology:measurement(mihna_quran_created_grid_31, quran_ontological_status__state_enforced_creation_reading, suppression(structural), 833, 0.5).
narrative_ontology:measurement_basis(mihna_quran_created_grid_31, observed).
narrative_ontology:measurement(mihna_quran_created_grid_32, quran_ontological_status__state_enforced_creation_reading, suppression(structural), 851, 0.2).
narrative_ontology:measurement_basis(mihna_quran_created_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Mu'tazilite doctrine that the Qur'an is created' decomposes into two structurally distinct claims with different epsilon values: the metaphysical thesis itself (sibling story quran_ontological_status__created_reading, whose burden is scholarly contestation only) and the enforcement arrangement erected upon it (this story, which additionally prices tribunals, imprisonment, and office purges). The third sibling, quran_ontological_status__uncreated_reading, instantiates the opposing metaphysical commitment as held by the refusing party. Direction of influence runs upstream-to-downstream: the doctrinal thesis supplies the content; the enforcement arrangement converts content into a suppression mechanism — which is why this story links both siblings in its network surface. Each member of the family carries a single stable epsilon over its own referent, per the decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
