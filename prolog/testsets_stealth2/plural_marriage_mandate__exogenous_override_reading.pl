% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: The 1890 Manifesto as Coerced Abandonment of a Divine Requirement (Exogenous Override Reading)
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   Between the Edmunds-Tucker Act (1887) and the Smoot-hearing purge era (c.
 *   1910), the Church of Jesus Christ of Latter-day Saints moved from open
 *   practice of plural marriage to formal abandonment via the 1890 Manifesto.
 *   THIS STORY instantiates the exogenous_override_reading of that history:
 *   the Manifesto was not a legitimate doctrinal reinterpretation but a
 *   surrender extracted by overwhelming state force — imprisonment of over a
 *   thousand men, corporate dissolution, property escheatment, and
 *   disfranchisement — imposed on a community that held the practice to be a
 *   divinely mandated, eternally binding requirement. On this reading the
 *   standing arrangement under contest (the epsilon referent) is the
 *   Manifesto compliance regime itself: the requirement that the faithful
 *   abandon the practice, administered domestically by the church under
 *   federal ultimatum. The other readings of the same kernel are separate
 *   constraints with their own epsilon values and are linked, not averaged,
 *   here. The claim/metric independence rule is honored deliberately:
 *   claimed_type is snare because this reading holds the coordination story
 *   (peaceful voluntary revelation) to be cover for coercive transfer, and
 *   the metrics are authored from the documented enforcement record — the
 *   engine computes per-seat classifications from the structural data, and
 *   divergence between any seat's computed type and this claim is signal, not
 *   error.
 *
 * KEY AGENTS:
 *   - federal_government: Agenda-setter (institutional/arbitrage) — prosecutes, seizes, disfranchises, and sets the terms of conformity; collects the settlement in the form of jurisdictional supremacy and territorial uniformity
 *   - lds_church_first_presidency: Coerced administrator (institutional/constrained) — issues the Manifesto under ultimatum, then enforces compliance domestically; bears doctrinal loss and schism while collecting survival, returned property, and statehood
 *   - practicing_polygamists: Primary target (moderate/constrained) — roughly 1,300 imprisoned; remaining choices are prison, underground life, exile to the Mexican and Canadian colonies, or abandonment of the practice
 *   - plural_wives_and_children: Primary target (powerless/trapped) — marriages stripped of legal recognition, social stigma, economic precarity; excluded from the settlement that disposed of their family structure
 *   - fundamentalist_dissenters: Secondary target (powerless/identity_locked) — refuse the override as void, continue the practice, and bear excommunication after 1904
 *   - anti_polygamy_reform_coalition: Beneficiary (organized/mobile) — Protestant churches, moral reform associations, and national press supplying the political demand; receives the conformity outcome
 *   - constitutional_historians: Analytical observer (analytical/analytical) — reconstruct the enforcement record and decision sequence from archives outside every benefiting party
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.82).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.72).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "The 1890 Manifesto as Coerced Abandonment of a Divine Requirement (Exogenous Override Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '452569f8-507d-469d-aee3-bb650026f6cd').
narrative_ontology:cs_kernel_codification('452569f8-507d-469d-aee3-bb650026f6cd', fixed_text).
narrative_ontology:cs_authority_grounding('452569f8-507d-469d-aee3-bb650026f6cd', lineage).
narrative_ontology:cs_interpretation_layer_present('452569f8-507d-469d-aee3-bb650026f6cd').
narrative_ontology:cs_reading_relation('452569f8-507d-469d-aee3-bb650026f6cd', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('452569f8-507d-469d-aee3-bb650026f6cd', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('452569f8-507d-469d-aee3-bb650026f6cd', foundational, divine_command_not_abrogable_by_state_force).
narrative_ontology:cs_axiom_status(divine_command_not_abrogable_by_state_force, holdable).
narrative_ontology:cs_axiom_grounding('452569f8-507d-469d-aee3-bb650026f6cd', divine_command_not_abrogable_by_state_force, theological).
narrative_ontology:cs_axiom('452569f8-507d-469d-aee3-bb650026f6cd', secondary, duress_voids_revelatory_authority).
narrative_ontology:cs_axiom_status(duress_voids_revelatory_authority, holdable).
narrative_ontology:cs_axiom_grounding('452569f8-507d-469d-aee3-bb650026f6cd', duress_voids_revelatory_authority, theological).
narrative_ontology:cs_reference_frame('452569f8-507d-469d-aee3-bb650026f6cd', divine_plural_marriage_mandate).
narrative_ontology:cs_drift_state('452569f8-507d-469d-aee3-bb650026f6cd', post_manifesto_smoot_hearings_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('452569f8-507d-469d-aee3-bb650026f6cd', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_reform_coalition).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, plural_wives_and_children).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, fundamentalist_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, lds_church_first_presidency).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, federal_police_power_over_marriage_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, religious_practice_subordination_precedent).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, territorial_uniformity_of_marriage_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the terms of settlement: Congress passes the Edmunds and Edmunds-Tucker Acts, federal marshals arrest roughly 1,300 men for unlawful cohabitation, the Supreme Court sustains the statutes (Reynolds 1879; Late Corporation of the Church 1890), the church's corporate charter is dissolved and its property above the statutory threshold escheated, and offenders are disfranchised. Its instruments are adjustable — statutes, judicial appointments, amnesties, and the statehood gate itself — so it can escalate or relent at will. It collects the outcome it demanded: uniform marriage law across the territories and undisputed jurisdiction.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Protestant denominations, moral reform associations, and the national press supply four decades of political demand for suppression, framing plural marriage as barbarism requiring federal intervention. They bear little direct cost of enforcement and receive the conformity outcome: the practice publicly ends and the territory enters the union on national moral terms. Their attention can move to other causes once the objective is achieved.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_reform_coalition, beneficiary,
    organized, generational, mobile, national).

% Issues the 1890 declaration under explicit threat of corporate dissolution and total property forfeiture, then administers compliance: leaders previously in hiding resume public roles, post-Manifesto marriages are curtailed, and after the 1904 Senate hearings the presidency adopts the Second Manifesto, disciplining and eventually excommunicating those who continue the practice. It regains most escheated property by 1893-94 and obtains statehood in 1896. It bears the doctrinal loss of abandoning a publicly announced eternal principle, the defection of dissenting members, and the permanent interpretive burden of explaining the reversal.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, lds_church_first_presidency, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, lds_church_first_presidency, payer).

% Men living in plural marriage face indictment, imprisonment, and disfranchisement; many serve penitentiary terms while church welfare supports their families. Their option set: remain and risk arrest, live underground, relocate to the church colonies in Mexico or Canada where federal process does not reach, or cease cohabitation with all but one wife. Community networks provide lawyers, concealment, and material aid, but cannot stop prosecution.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    moderate, biographical, constrained, regional).

% Wives in plural marriages lose whatever legal recognition their status ever had; children of such unions carry stigma and uncertain inheritance standing. Economic security depends on households the enforcement campaign targets. None of the negotiating parties represents them: the settlement that disposes of their family structure is concluded between federal prosecutors and church leadership, and they have no seat at either table.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, plural_wives_and_children, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, plural_wives_and_children, excluded).

% Members who hold the plural-marriage covenant to be eternally binding and the 1890 declaration void reject the settlement outright. Some contract post-Manifesto marriages; after 1904 the church disciplines and excommunicates them, and they organize outside it (the Council of Friends lineages and their successors). Leaving the practice would mean abandoning what they understand as an eternal covenant with salvific consequence, so their membership in the dissent is constitutive of identity rather than a chosen affiliation; they bear excommunication, social marginalization, and later legal prosecution across the twentieth century.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, fundamentalist_dissenters, payer,
    powerless, civilizational, identity_locked, regional).

% Reconstruct the sequence — statutes, prosecutions, the negotiation trail preceding the declaration, diary and archival evidence, the Smoot testimony — from sources outside every benefiting party. They assess whether the declaration's framing as revelation matches its documented production circumstances, and quantify the post-Manifesto marriage population against the professed abandonment.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement synchronizes a single marriage-law standard across the territories: it resolves the collision between ecclesiastical sanction of plural marriage and federal criminal law by bringing the church's domestic practice under the federal standard, ending jury nullification, witness intimidation, and the enforcement impossibility in Utah Territory, and clearing the path to statehood.
% TRANSFER_FUNCTION: Moves religious practice, family legal recognition, and personal liberty from practicing polygamists and their families to the federal government (jurisdictional supremacy, territorial uniformity) and to the church institution (continued legal existence, returned property, statehood), with the anti-polygamy coalition collecting the moral-conformity outcome it spent forty years demanding.
% ABSENT_VOICES: Plural wives and their children had no seat in the settlement that disposed of their family structure; the imprisoned polygamists were excluded from the decision that surrendered the practice defining their households; the fundamentalist believers who would reject the declaration as void were not consulted and learned of it by announcement. They were in prison, in exile colonies, or silenced within the community — and after 1904, formally expelled.
% DISAPPEARANCE_RATIONALE: If the Manifesto regime vanished overnight, the jurisdictional war resumes: federal prosecution restarts against a church returning to open practice, the statehood settlement unwinds, the returned-property arrangement is destabilized, and the fundamentalist dissent loses the grievance that organizes it. Every party's current arrangement — federal uniformity, church incorporation, reform victory, dissident identity — depends on the regime holding.
% FOUNDING_PROBLEM: An irreconcilable jurisdiction: a territory-governed church publicly sanctioned as divine duty exactly what federal criminal law forbade, making enforcement impossible (juries would not convict, witnesses would not testify) and blocking Utah statehood until either the law or the practice yielded.
% FOUNDING_PROBLEM_CORROBORATION: The political layer is corroborated from outside every benefiting party: Supreme Court opinions (Reynolds v. United States 1879; Late Corporation of the Church v. United States 1890), congressional debate records, contemporaneous non-Mormon press, and modern constitutional histories all attest the enforcement campaign and the jurisdictional crisis it addressed. The theological layer — whether the original mandate was in fact divinely binding and whether the 1890 declaration carried revelatory authority — is attested only by believing parties on each side (the church's official account versus the dissenters' archives); no source outside the contesting faith communities can corroborate a divine-status claim, and that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82 at interval end) because on this reading the arrangement removes a practice the community held to be eternally required, along with the legal existence of existing plural families — a total transfer of religious practice, family integrity, and liberty. Suppression is authored 0.72 as a raw structural property (unscaled by power or scope): the external machinery peaked around 1887-1890 and then wound down, but enforcement migrated inward — disciplinary councils, temple-recant denials, and finally excommunication — while the internalized obedience mechanism sustained compliance without external penalty. Theater_ratio peaks at 0.70 in 1904 because the professed abandonment coexisted with a documented population of post-Manifesto plural marriages: the largest gap between the arrangement's stated function and its operation sits at the Second Manifesto. Accessibility_collapse is 0.62: alternatives did not vanish but narrowed to costly residuals — underground practice, exile colonization, or schismatic exit. Resistance is 0.68: jury nullification, witnesses refusing to testify, leaders in hiding, continued secret marriages, and ultimately the fundamentalist schism itself. The measurement series run on one single shared time grid (all three metrics authored at all eight points) so the engine never substitutes an end-state scalar into an earlier row. The trajectories show a two-stage ratchet: external coercion crests and recedes (1887-1896) while extraction consolidates, then a second internal ratchet (1904-1910) completes the purge of dissent. This is a ratchet, not a cycle — each relaxation of federal pressure was followed by consolidation of the surrender, not reversal.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical facts. From the federal seat, this is lawful enforcement of a neutral criminal statute validated by Reynolds v. United States (1879) — near-zero experienced extraction, a routine police-power operation. From the practicing-polygamist seat, the same events are the destruction of a divine obligation and of legally recognized family life under threat of imprisonment — maximal experienced extraction with constrained exit. From the First Presidency seat, the arrangement is a coerced trade: doctrine and dissenters paid out, corporate survival and statehood collected — a mixed profile no single scalar captures, which is why the church carries a dual role. Among same-level payer seats, exit options differentiate outcomes despite equal nominal standing: practicing polygamists held constrained exit (exile colonies existed), plural wives were trapped (no recognized marital status anywhere to exit into), and fundamentalist dissenters were identity_locked (defiance constituted their covenant identity, making submission the only exit and therefore no exit at all). The engine computes these divergences from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the federal government (receives conformity and jurisdictional supremacy; d near the beneficiary end, modestly amplified by national scope) and for the anti-polygamy reform coalition (receives the moral-conformity outcome it demanded; mobile exit keeps it nearest the subsidy end). Victim declarations drive high directionality for practicing polygamists, plural wives and children, and fundamentalist dissenters; trapped and identity_locked exit positions push those seats toward the full-target end, so effective extraction is amplified well above base epsilon for them. The First Presidency is the deliberate dual case: it administers and enforces the arrangement domestically (agenda-setter pull toward low d) while bearing doctrinal loss and losing its dissenting members (payer pull toward high d); no override is authored because the derivation from the dual role plus constrained exit is the honest representation — the ambiguity is structural, not a derivation failure. Vindicated propositions (the federal police-power-over-marriage doctrine and the religious-practice-subordination precedent) are recorded separately: they collect no rents and are not beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an irreconcilable jurisdiction in which ecclesiastical authority sanctioned what federal criminal law forbade, in a territory whose statehood depended on resolution — was politically resolved by 1896. Yet the arrangement did not sunset: it intensified internally after 1904, purging the members whose practice the original settlement had nominally grandfathered. This is the mismatch signature: founding_problem_status contested (politically dead, theologically live for the dissenting seats) crossed with disappearance_verdict world_rearranges. The classification prevents the two symmetric mislabels. Read without reading-indexed epsilon, the Manifesto looks like benign doctrinal evolution — a rope or scaffold of peaceful adaptation — which launders the coercion out of the record. Read as pure timeless oppression, it misses that the arrangement has a specific founding problem, a specific settlement, and a specific second ratchet that outlived the first. The snare claim is indexed to this reading's referent: the compliance regime as imposed on a still-binding divine command. Whether that referent description survives contact with the sibling readings is precisely what the kernel omega routes to investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_of_disagreement,
    'This constraint is ONE reading (exogenous_override_reading) of the kernel plural_marriage_mandate. The disagreement among readings is located at the causal origin and legitimacy of the 1890 suspension: was the plural-marriage requirement rescinded by God (endogenous_reinterpretation_reading), overridden by state force against a still-binding command (this reading), or was the revelation narrative strategic cover for survival-driven capitulation (institutional_pragmatism_reading)? What would adopting a sibling reading change structurally?',
    'Comparative structural analysis across the three reading-stories: the endogenous reading shrinks the victim set to near-zero (obedience, not extraction) and shifts type toward rope or scaffold; the pragmatist reading keeps the transfer but relocates agency inside the church hierarchy, shifting the agenda_setter seat. The locus of dispute is the authenticity and authority of the 1890 declaration itself.',
    'If the endogenous reading is adopted, this story''s victims dissolve into voluntary participants and epsilon collapses toward coordination cost; if the pragmatist reading is adopted, the victim set persists but the federal seat demotes from agenda_setter to mere background condition. This story''s classification is valid only within its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Committer-frame omega: one-of-three readings of the plural marriage mandate kernel; sibling readings redistribute victims, beneficiaries, and the agenda-setting seat.').

omega_variable(
    woodruff_declaration_authenticity,
    'Was Woodruff''s 1890 declaration a genuine revelatory event (which would dissolve this reading''s coercion claim) or a document produced under duress whose revelatory framing was retroactive?',
    'Contemporaneous documentary evidence independent of later institutional framing: Wilford Woodruff''s diaries and the textual history of the 1886 revelation, the sequence of negotiations with federal officials preceding the declaration, and testimony from the Smoot hearing record.',
    'If the declaration is authentic revelation, this reading''s epsilon referent collapses and the constraint reclassifies toward the endogenous sibling''s profile; if documentary evidence shows drafting under explicit threat of dissolution, the coercion reading is confirmed and the measured extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(woodruff_declaration_authenticity, empirical, 'Authenticity of the 1890 declaration as revelation versus coerced instrument.').

omega_variable(
    suppression_internalization_split,
    'How much of the measured suppression is structural (federal penalties, property forfeiture, disfranchisement) versus internalized (the doctrine of prophetic infallibility making defiance unthinkable even after external penalties were lifted)?',
    'Post-amnesty compliance trajectory: federal prosecution largely ceased after 1896, yet compliance held and hardened after 1904 under purely ecclesiastical discipline. If compliance persists with external penalties removed, the internalized share is high; the trajectory after 1904 is the natural experiment.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure and persists independently of federal capacity — the constraint survives the withdrawal of its original enforcer, which stabilizes the snare classification even as the federal machinery winds down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized components of the suppression sustaining Manifesto compliance.').

omega_variable(
    post_manifesto_marriage_population,
    'How many plural marriages were actually contracted after the 1890 declaration, given that some continued secretly in Mexico, Canada, and on international waters until at least 1904?',
    'Genealogical and archival reconstruction of post-1890 sealings and civil records from the colony archives; estimates in the historiography range from dozens to several hundred.',
    'A large hidden population raises the honest theater_ratio (professed abandonment versus continued practice) and shows the Manifesto''s stated function was substantially performed rather than real during 1890-1904; a small population supports treating the declaration as functionally operative from issuance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_manifesto_marriage_population, empirical, 'Scale of continued plural marriage after the Manifesto, governing the honesty of the abandonment claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1887, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1887, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1887, 0.3).
narrative_ontology:measurement_basis(plur_tr_t1887, observed).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.5).
narrative_ontology:measurement_basis(plur_tr_t1890, observed).
narrative_ontology:measurement(plur_tr_t1893, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1893, 0.55).
narrative_ontology:measurement_basis(plur_tr_t1893, observed).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1896, 0.6).
narrative_ontology:measurement_basis(plur_tr_t1896, observed).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1900, 0.62).
narrative_ontology:measurement_basis(plur_tr_t1900, observed).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1904, 0.7).
narrative_ontology:measurement_basis(plur_tr_t1904, observed).
narrative_ontology:measurement(plur_tr_t1907, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1907, 0.66).
narrative_ontology:measurement_basis(plur_tr_t1907, observed).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1910, 0.6).
narrative_ontology:measurement_basis(plur_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(plur_be_t1887, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1887, 0.7).
narrative_ontology:measurement_basis(plur_be_t1887, observed).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.74).
narrative_ontology:measurement_basis(plur_be_t1890, observed).
narrative_ontology:measurement(plur_be_t1893, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1893, 0.73).
narrative_ontology:measurement_basis(plur_be_t1893, observed).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1896, 0.71).
narrative_ontology:measurement_basis(plur_be_t1896, observed).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1900, 0.72).
narrative_ontology:measurement_basis(plur_be_t1900, observed).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1904, 0.78).
narrative_ontology:measurement_basis(plur_be_t1904, observed).
narrative_ontology:measurement(plur_be_t1907, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1907, 0.8).
narrative_ontology:measurement_basis(plur_be_t1907, observed).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1910, 0.82).
narrative_ontology:measurement_basis(plur_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1887, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1887, 0.85).
narrative_ontology:measurement_basis(plur_su_t1887, observed).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.8).
narrative_ontology:measurement_basis(plur_su_t1890, observed).
narrative_ontology:measurement(plur_su_t1893, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1893, 0.65).
narrative_ontology:measurement_basis(plur_su_t1893, observed).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1896, 0.55).
narrative_ontology:measurement_basis(plur_su_t1896, observed).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement_basis(plur_su_t1900, observed).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1904, 0.62).
narrative_ontology:measurement_basis(plur_su_t1904, observed).
narrative_ontology:measurement(plur_su_t1907, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1907, 0.58).
narrative_ontology:measurement_basis(plur_su_t1907, observed).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1910, 0.52).
narrative_ontology:measurement_basis(plur_su_t1910, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the 1890 Manifesto' decomposes into three structurally distinct constraints — one per reading of the plural_marriage_mandate kernel — per the epsilon-invariance principle. The endogenous reading is the upstream account (issued by the authority whose legitimacy the kernel carries; it supplies the official narrative that the other two readings react against). This exogenous_override story is downstream: it accepts the enforcement record as fact and contests only the legitimacy layer, so its epsilon is authored over the same standing arrangement the endogenous reading describes as voluntary. The institutional_pragmatism reading shares this story's factual substrate but relocates agency inside the church hierarchy. Each file carries its own beneficiaries, victims, and claimed type; linking them via affects_constraints lets contamination analysis track how evidence bearing on the declaration's authenticity propagates across all three classifications simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
