% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: Plural Marriage as Irrevocable Eternal Law (Immutable Commandment Reading)
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   Between the 1852 public announcement and the 1890 Manifesto, the Church
 *   of Jesus Christ of Latter-day Saints administered plural marriage as an
 *   eternal, immutable divine requirement for the highest degree of
 *   salvation, grounded in the 1843 revelation canonized as Doctrine and
 *   Covenants 132. The arrangement welded a gathered, persecuted convert
 *   population into a covenant polity with dense elite kinship networks,
 *   sustained boundary distinctiveness, and drove settlement across the
 *   Intermountain West, while concentrating marriage capacity and household
 *   authority in senior priesthood hands, leaving plural wives legally
 *   unrecognized, displacing young men from the marriage market, disciplining
 *   dissent, and exposing the entire community to felony prosecution,
 *   imprisonment, disfranchisement, and asset forfeiture under successive
 *   federal statutes. By the 1880s the arrangement ran on a martyrdom
 *   dynamic: keeping the covenant meant civil criminality, and keeping the
 *   law of the land meant abandoning the covenant, with no legitimate
 *   procedure inside this reading for releasing either side. KEY AGENTS (by
 *   structural relationship): lds_first_presidency_quorum_of_twelve - primary
 *   agenda setter (institutional/identity_locked), administers the covenant
 *   and collects authority-legitimation; plural_household_priesthood_holders
 *   - primary beneficiary (organized/identity_locked) with payer liabilities;
 *   plural_wives - primary target (powerless/constrained), legal erasure and
 *   economic dependence; unmarried_young_men - secondary target
 *   (powerless/mobile), marriage-market displacement;
 *   rank_file_latter_day_saints - diffuse bearers (organized/identity_locked)
 *   of persecution costs and recipients of community goods;
 *   dissenters_and_apostates - disciplined exit-seekers
 *   (moderate/constrained); united_states_federal_government - external
 *   coercive agenda setter (institutional/mobile). The eps referent for this
 *   kernel-reading story is the standing plural-marriage order itself,
 *   assessed by this reading's own lights, never any successor arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.74).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.88).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "Plural Marriage as Irrevocable Eternal Law (Immutable Commandment Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, 'd70400ae-773d-48ed-b15a-f47a7887e41e').
narrative_ontology:cs_kernel_codification('d70400ae-773d-48ed-b15a-f47a7887e41e', fixed_text).
narrative_ontology:cs_authority_grounding('d70400ae-773d-48ed-b15a-f47a7887e41e', lineage).
narrative_ontology:cs_interpretation_layer_present('d70400ae-773d-48ed-b15a-f47a7887e41e').
narrative_ontology:cs_reading_relation('d70400ae-773d-48ed-b15a-f47a7887e41e', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('d70400ae-773d-48ed-b15a-f47a7887e41e', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('d70400ae-773d-48ed-b15a-f47a7887e41e', foundational, plural_marriage_required_for_exaltation).
narrative_ontology:cs_axiom_status(plural_marriage_required_for_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('d70400ae-773d-48ed-b15a-f47a7887e41e', plural_marriage_required_for_exaltation, theological).
narrative_ontology:cs_axiom('d70400ae-773d-48ed-b15a-f47a7887e41e', foundational, canonical_revelation_admits_no_supersession).
narrative_ontology:cs_axiom_status(canonical_revelation_admits_no_supersession, holdable).
narrative_ontology:cs_axiom_grounding('d70400ae-773d-48ed-b15a-f47a7887e41e', canonical_revelation_admits_no_supersession, theological).
narrative_ontology:cs_reference_frame('d70400ae-773d-48ed-b15a-f47a7887e41e', irrevocable_abrahamic_restoration_order).
narrative_ontology:cs_drift_state('d70400ae-773d-48ed-b15a-f47a7887e41e', manifesto_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('d70400ae-773d-48ed-b15a-f47a7887e41e', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, lds_first_presidency_quorum_of_twelve).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, plural_household_priesthood_holders).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, plural_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, unmarried_young_men).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, rank_file_latter_day_saints).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, dissenters_and_apostates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, rank_file_latter_day_saints).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, plural_household_priesthood_holders).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, abrahamic_restoration_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, sealing_keys_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Presides over the church, announces and defends the doctrine publicly beginning with the 1852 proclamation, administers sealings and temple ordinances, disciplines members who refuse or criticize the marriage order, and directs the community's response to federal prosecution. Collects the deference, tithing loyalty, and authority-legitimation that flow from administering an unconditional divine requirement. Cannot renounce the doctrine without dissolving the claim to prophetic authority on which its office rests; during the 1880s raid its senior members live in hiding.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, lds_first_presidency_quorum_of_twelve, agenda_setter,
    institutional, generational, identity_locked, continental).

% Senior men sealed to multiple wives under the covenant. Receive enlarged households, kinship alliances with other leading families, and standing in the celestial order as the reading prices it. Bear the matching liabilities: felony prosecution, prison terms, hiding on the underground, and separation from portions of their families during raids.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_household_priesthood_holders, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, plural_household_priesthood_holders, payer).

% Women sealed into plural households. Carry legal nonexistence as wives under federal and territorial statute, economic dependence on household administration they do not govern, shared conjugal life on terms the husband sets, and the community expectation that acceptance secures exaltation for themselves and their children. Divorce exists but is rare, costly, and socially catastrophic; leaving the community forfeits the sealed standing the entire order turns on.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_wives, payer,
    powerless, biographical, constrained, regional).

% Young single men in a marriage market where senior men hold multiple wives. Face deferred or forgone marriage, reduced prospects for household formation inside Utah, and a choice between waiting, emigrating to the Mexico and Canada colonies where the balance differs, or leaving the community altogether.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, unmarried_young_men, payer,
    powerless, biographical, mobile, regional).

% Ordinary members who mostly hold no plural seals but fund, house, and shield the practice and absorb its consequences: husbands and fathers imprisoned for unlawful cohabitation, property confiscated under the Edmunds-Tucker Act, electoral disfranchisement, and children raised amid raids. Receive the community goods the covenant order sustains: mutual aid, gathered identity, and a framework of meaning under persecution.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, rank_file_latter_day_saints, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, rank_file_latter_day_saints, beneficiary).

% Members who reject the marriage order or its enforcement, including Godbeite liberals, RLDS-aligned kin, and quiet refusers. Subject to church discipline, social ostracism, and loss of standing. Public objection generally requires exiting to Gentile venues, at the cost of family rupture and, per the reading's own terms, salvation itself.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissenters_and_apostates, payer,
    moderate, biographical, constrained, regional).

% Congress, the presidency, territorial appointees, and federal marshals administering the anti-bigamy regime: the Morrill Act, Poland Act, Edmunds Act, and Edmunds-Tucker Act. Criminalizes the marriage order, imprisons practitioners, seizes church property, and disfranchises Utah women, acting on a jurisdictional claim the covenant's own authority structure refuses to recognize. Its escalating pressure is the term that turns the doctrine's demand into a martyrdom test.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, united_states_federal_government, agenda_setter,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, lds_first_presidency_quorum_of_twelve).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bound a heterogeneous gathering of converts into a single covenant polity with dense elite kinship networks; sustained boundary distinctiveness and internal loyalty under external attack; organized the marriage economy, household formation, and settlement expansion of the Intermountain colonies.
% TRANSFER_FUNCTION: Moved marriage capacity, household labor, and reproductive obligation from women and younger men toward senior priesthood holders; moved legal jeopardy, imprisonment, and asset-forfeiture exposure onto the whole member body during the federal confrontation; moved obedience and resources upward to the presiding quorums.
% ABSENT_VOICES: Uncoerced wives' voices: objection channels ran through husbands and bishops, and pressing dissent risked standing and, per the reading, salvation. Young men displaced from the marriage economy had no seat in the councils setting the marriage economy's terms. The federal government's jurisdictional claim had zero standing inside the covenant's authority structure even as its prosecutions decided the arrangement's fate. Public dissent therefore required exiting the community to Gentile courts, press, or lecture circuits to be heard at all.
% DISAPPEARANCE_RATIONALE: Overnight removal would normalize Utah's marriage order within a generation, dissolve the sealing-based kinship politics that organized the elite, remove the object of the federal confrontation before the prison wave and property seizures mature, and eliminate the schismatic custody fight over the doctrine's status. The distinctive covenant identity would reorganize around other markers such as gathering, temple worship, and dietary law.
% FOUNDING_PROBLEM: Restore the Abrahamic patriarchal order and seal a covenant people whose marriages and lineage bind them to exaltation, welding a dispersed convert population into a durable theocratic community on the frontier.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by federal prosecutorial and judicial records (Reynolds v. United States filings document the doctrine's centrality and adherents' willingness to suffer for it), contemporary Gentile journalism, and apostate memoirs (Ann Eliza Young, Fanny Stenhouse), all attesting that adherents held the eternal-law conviction at existential cost while disputing the conviction's content. No neutral insider attestation exists: inside voices defending the founding problem were all beneficiaries of or dependents on the arrangement.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74 at interval end) is authored over the standing arrangement as this reading sees it: the reading prices the costs as covenant sacrifice toward exaltation, which damps but does not erase measured asymmetry, because the reading's own discourse registers who decides, who consents under salvation stakes, and who bears legal exposure. The rising trajectory tracks extraction accumulation: as federal pressure escalated, the covenant became a communal liability consuming the body it bound, and the martyrdom framing is the reading's own way of registering maximal cost while pricing it as victory. Suppression (0.88, raw and unscaled per the framework rule) is dual-sourced: internal enforcement (disciplinary councils, social sanction, salvation-stakes pricing of refusal) compounded by external federal criminality that closed the practical exit space. Theater (0.44 end-state) starts low because the practice was substantively performed, real marriages and real governance throughout, and climbs late as the gap widened between professed immovability in public settings and frantic backstage negotiation, petition drafting, and succession hedging. Accessibility_collapse (0.70) reflects alternatives that existed but carried catastrophic pricing once the reading was accepted: RLDS affiliation, emigration, quiet noncompliance. Resistance (0.62) reflects internal dissent movements and organized external opposition. The claimed type is tangled_rope from structural judgment: genuine coordination function, asymmetric costs, and enforcement-dependence are all load-bearing. The metrics are authored independently as descriptive fact. The series run on one shared eight-point time grid; the trajectory is a monotonic ratchet, not a cycle.
 *
 * PERSPECTIVAL GAP:
 *   The two agenda-setter seats sit in contention at equal institutional power and should compute oppositely: the hierarchy experiences the arrangement as covenant administration whose persecution vindicates its authority claim, while the federal seat experiences the same arrangement as a criminal enterprise to be suppressed. Among payer seats the divergence is starker still: plural wives experienced legal erasure inside a community that publicly celebrated their station; rank-and-file saints absorbed prison and forfeiture as the price of belonging; young men experienced the marriage market as a closed queue. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the church hierarchy and plural-household holders toward the beneficiary end of d; the four victim declarations drive plural wives, young men, rank-and-file saints, and dissenters toward the target end, with the dual-positioned seats (plural-household holders, rank-and-file saints) sitting mid-scale because they both receive community goods and bear liabilities. The federal government is the hard case: it is an agenda setter for a rival regime, not a collector under this one, so a naive derivation from its agenda_setter role could misplace it near the beneficiary end. No directionality override is authored because overrides key on power_atom, and the two institutional adversaries (church hierarchy and federal government) share the institutional atom; an override would collide across them. The hierarchy's placement is handled correctly by its presence in beneficiaries[], and the federal seat's adversarial position is carried by its situation text. Coalition potential among the payer seats existed in principle but was blocked structurally: salvation-stakes pricing made open refusal self-condemning, household surveillance and bishop interviews monitored wives' alignment, and the most aggrieved payers (young men) were geographically dispersible rather than concentratable.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading blocks mandatrophy resolution by design: an eternal mandate cannot outlive its function inside its own frame, so the apparatus detects the trap rather than the obsolescence. The tangled_rope classification prevents the two standard mislabels. It prevents the rope mislabel: enforcement is load-bearing (refusal was disciplined, exits were punished), so this is not costless coordination. It prevents the snare mislabel: the coordination function was genuine and sincerely held, corroborated behaviorally by adherents enduring prison rather than renouncing, so the coordination story is not mere cover. The concentrated capture (gain_flow names the presiding quorums) and prohibitive fixing cost rule out the piton reading: this was not inertial residue but a defended, profitable arrangement whose custodians chose hiding over revision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates only the immutable_commandment_reading of kernel eternal_marriage_covenant; how would the sibling readings (prophetic_override_reading, temporal_accommodation_reading) restructure the beneficiary and victim surface and the revision path?',
    'Generate and compare the sibling stories: the prophetic_override_reading legitimizes revision, removing the no-revision trap and shifting enforcement from covenant discipline to interpretive discretion; the temporal_accommodation_reading splits divine law from civil compliance, converting the martyrdom dynamic into dual-obligation management and shrinking the victim set to those harmed during the suspension gap.',
    'Under the override reading the arrangement''s persistence stops depending on persecution-endurance and becomes contingent on interpretive choice; under the accommodation reading epsilon drops because adherence no longer requires civil criminality; the immutable reading uniquely generates the martyrdom and no-revision structure measured here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the eternal_marriage_covenant kernel; sibling comparisons pending.').

omega_variable(
    no_legitimate_revision_path_status,
    'Did any legitimate revision mechanism exist inside the immutable-commandment framework, or did the 1890 suspension operate as an unacknowledged breach of the framework''s own rule?',
    'Trace post-Manifesto succession disputes and the fundamentalist movement''s custody claims over D&C 132: if organized bodies asserted the law remained binding and the suspension illegitimate, the framework contained no self-owned revision valve and ended only by breaking its own authority rule.',
    'Confirms the martyrdom structure and predicts schismatic residue: the constraint''s text lives on after its enforcement dies, held by continuator communities that inherit the unacknowledged breach rather than a completed revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(no_legitimate_revision_path_status, empirical, 'Whether the no-revision-path feature is structural to the reading or contingent on particular officeholders.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (salvation-stakes discipline, community boundary enforcement, federal criminality) or internalized (covenant identity fusion, taught wifely duty, learned worthlessness on refusal)?',
    'Post-exit trajectory of leavers: examine apostate memoirs and RLDS conversion accounts for whether dread, salvation-fear, and family-obligation distress persist after physical exit and external danger end; persistence indicates a large internalized component.',
    'If substantially internalized, effective suppression exceeds the structural measure and survives the arrangement''s formal end, predicting multi-generation residue in continuator communities and family rupture rather than clean termination at 1890.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between external enforcement and internalized covenant identity fusion.').

omega_variable(
    consent_conditions_under_salvation_stakes,
    'To what extent did plural wives'' acceptance reflect autonomous religious conviction versus coercion operating through salvation stakes, family pressure, and economic dependence?',
    'Systematic reading of women''s diaries and correspondence (Emmeline B. Wells, Zina Huntington, Annie Clark Tanner) against the choice sets actually available at each acceptance decision, distinguishing conviction voiced under genuine alternatives from assent under closure.',
    'A high coerced-consent share raises the extractiveness attributable to the marriage form itself; a low share shifts attribution toward the surrounding legal and institutional environment, changing which remedial lever the classification implicates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_conditions_under_salvation_stakes, empirical, 'Attribution of measured extraction between covenant acceptance and coercive context.').

omega_variable(
    cohesion_form_separability,
    'Was the community-binding coordination function separable from the plural-marriage form, or did the cohesion depend on the contested marriage practice specifically?',
    'Compare boundary-solidarity outcomes in contemporaneously persecuted covenant communities that did not adopt plural marriage, and in LDS subgroups that rejected it (RLDS): comparable solidarity without the form indicates separability.',
    'If separable, the marriage-specific structure is predominantly extraction riding a general covenant coordination; if inseparable, part of measured epsilon is the irreducible price of the coordination itself, strengthening the tangled_rope reading against the snare reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cohesion_form_separability, conceptual, 'Whether the coordination component depends on the contested marriage form or generalizes across covenant mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 1852, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1852, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1852, 0.12).
narrative_ontology:measurement_basis(eter_tr_t1852, observed).
narrative_ontology:measurement(eter_tr_t1857, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1857, 0.14).
narrative_ontology:measurement_basis(eter_tr_t1857, observed).
narrative_ontology:measurement(eter_tr_t1862, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1862, 0.16).
narrative_ontology:measurement_basis(eter_tr_t1862, observed).
narrative_ontology:measurement(eter_tr_t1870, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1870, 0.19).
narrative_ontology:measurement_basis(eter_tr_t1870, observed).
narrative_ontology:measurement(eter_tr_t1879, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1879, 0.24).
narrative_ontology:measurement_basis(eter_tr_t1879, observed).
narrative_ontology:measurement(eter_tr_t1882, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1882, 0.3).
narrative_ontology:measurement_basis(eter_tr_t1882, observed).
narrative_ontology:measurement(eter_tr_t1886, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1886, 0.36).
narrative_ontology:measurement_basis(eter_tr_t1886, observed).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1890, 0.44).
narrative_ontology:measurement_basis(eter_tr_t1890, observed).

% Extraction over time
narrative_ontology:measurement(eter_be_t1852, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1852, 0.48).
narrative_ontology:measurement_basis(eter_be_t1852, observed).
narrative_ontology:measurement(eter_be_t1857, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1857, 0.52).
narrative_ontology:measurement_basis(eter_be_t1857, observed).
narrative_ontology:measurement(eter_be_t1862, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1862, 0.54).
narrative_ontology:measurement_basis(eter_be_t1862, observed).
narrative_ontology:measurement(eter_be_t1870, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1870, 0.57).
narrative_ontology:measurement_basis(eter_be_t1870, observed).
narrative_ontology:measurement(eter_be_t1879, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1879, 0.6).
narrative_ontology:measurement_basis(eter_be_t1879, observed).
narrative_ontology:measurement(eter_be_t1882, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1882, 0.66).
narrative_ontology:measurement_basis(eter_be_t1882, observed).
narrative_ontology:measurement(eter_be_t1886, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1886, 0.71).
narrative_ontology:measurement_basis(eter_be_t1886, observed).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1890, 0.74).
narrative_ontology:measurement_basis(eter_be_t1890, observed).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1852, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1852, 0.42).
narrative_ontology:measurement_basis(eter_su_t1852, observed).
narrative_ontology:measurement(eter_su_t1857, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1857, 0.5).
narrative_ontology:measurement_basis(eter_su_t1857, observed).
narrative_ontology:measurement(eter_su_t1862, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1862, 0.52).
narrative_ontology:measurement_basis(eter_su_t1862, observed).
narrative_ontology:measurement(eter_su_t1870, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1870, 0.58).
narrative_ontology:measurement_basis(eter_su_t1870, observed).
narrative_ontology:measurement(eter_su_t1879, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1879, 0.64).
narrative_ontology:measurement_basis(eter_su_t1879, observed).
narrative_ontology:measurement(eter_su_t1882, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1882, 0.74).
narrative_ontology:measurement_basis(eter_su_t1882, observed).
narrative_ontology:measurement(eter_su_t1886, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1886, 0.83).
narrative_ontology:measurement_basis(eter_su_t1886, observed).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1890, 0.88).
narrative_ontology:measurement_basis(eter_su_t1890, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the eternal marriage covenant.' This file instantiates the immutable_commandment_reading: epsilon authored over plural marriage as unconditional eternal obligation, with martyrdom dynamics and no legitimate revision path. The sibling stories instantiate the prophetic_override_reading (revision legitimacy changes the enforcement surface and removes the trap) and the temporal_accommodation_reading (law-of-the-land precedence shrinks the victim set and decouples doctrine from civil criminality). The immutable reading is the reference frame the other two respond to; its canonical text (D&C 132) is cited by all three, so contamination propagates upstream from any revision-legitimacy shift in the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
