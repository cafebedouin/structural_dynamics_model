% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Section 132 Doctrine/Practice Gap: Preserved Principle, Suspended Compliance
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story instantiates the practice_doctrine_gap reading of the
 *   marriage_commitment_reversal kernel: not that the reversal was purely
 *   coerced from outside (exogenous_override_reading), nor that it was a
 *   settled internal doctrinal revision
 *   (endogenous_reinterpretation_reading), but that the institution
 *   maintained a structural ambiguity — Section 132 remained canonical
 *   scripture, unretracted, while public compliance with monogamy was
 *   declared and selectively enforced. This gap is the constraint: it is what
 *   let roughly 200 plural marriages proceed in claimed-legal jurisdictions
 *   (Mexico, Canada, international waters) between 1890 and 1904 while the
 *   institution told Congress and the general public the practice had ended.
 *   The gap itself — not the Manifesto and not the revelation claim — is the
 *   extractive structure analyzed here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.71).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.62).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.71).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Section 132 Doctrine/Practice Gap: Preserved Principle, Suspended Compliance").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '34430fcb-5a6f-456b-a462-89daa06b2f10').
narrative_ontology:cs_kernel_codification('34430fcb-5a6f-456b-a462-89daa06b2f10', fixed_text).
narrative_ontology:cs_authority_grounding('34430fcb-5a6f-456b-a462-89daa06b2f10', extraction).
narrative_ontology:cs_interpretation_layer_present('34430fcb-5a6f-456b-a462-89daa06b2f10').
narrative_ontology:cs_reading_relation('34430fcb-5a6f-456b-a462-89daa06b2f10', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('34430fcb-5a6f-456b-a462-89daa06b2f10', marriage_commitment_reversal__endogenous_reinterpretation_reading, influences).
narrative_ontology:cs_axiom('34430fcb-5a6f-456b-a462-89daa06b2f10', foundational, doctrinal_preservation_permits_practice_suspension).
narrative_ontology:cs_axiom_status(doctrinal_preservation_permits_practice_suspension, holdable).
narrative_ontology:cs_axiom_grounding('34430fcb-5a6f-456b-a462-89daa06b2f10', doctrinal_preservation_permits_practice_suspension, conventional).
narrative_ontology:cs_axiom('34430fcb-5a6f-456b-a462-89daa06b2f10', secondary, unretracted_scripture_retains_binding_force_regardless_of_enforcement).
narrative_ontology:cs_axiom_status(unretracted_scripture_retains_binding_force_regardless_of_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('34430fcb-5a6f-456b-a462-89daa06b2f10', unretracted_scripture_retains_binding_force_regardless_of_enforcement, deontological).
narrative_ontology:cs_reference_frame('34430fcb-5a6f-456b-a462-89daa06b2f10', section_132_as_binding_eternal_principle).
narrative_ontology:cs_drift_state('34430fcb-5a6f-456b-a462-89daa06b2f10', post_manifesto_ambiguity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34430fcb-5a6f-456b-a462-89daa06b2f10', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, post_manifesto_plural_families_in_claimed_jurisdictions).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_adherents).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, wives_and_children_of_disavowed_marriages).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, prophetic_authority_over_scripture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the 1890 Manifesto's public declaration while never rescinding Section 132 as scripture. Controls which behavior is punished (public plural marriages after 1890) versus which is quietly tolerated or facilitated (marriages performed in Mexico, Canada, or on ships outside U.S. jurisdiction, roughly 200 between 1890 and 1904). Retains maximal doctrinal and organizational flexibility: can point to the Manifesto to satisfy federal authorities and Congress, and point to Section 132's undisturbed canonical status to satisfy members who read the suspension as tactical, not theological.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, church_institutional_leadership, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, church_institutional_leadership, beneficiary).

% Told publicly that plural marriage has ended in obedience to revelation, while rumors and evidence circulate that some Church authorities continued sanctioning it privately. Bears the confusion, the whiplash of loyalty tested by an unexplained gap between what leadership says and what leadership does, and later the reputational cost when the post-Manifesto marriages became public via the Smoot hearings. Exit means leaving the institution that structures their family, economic, and social life; most stay and absorb the ambiguity as a test of faith.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    moderate, biographical, constrained, national).

% Take the doctrinal preservation of Section 132 literally and continue or resume plural marriage practice on the premise that a temporary suspension is not a permanent revocation. Are excommunicated and denounced as apostate by the same institution whose own leadership sanctioned similar marriages within the ambiguity window. Have no institutional voice: their reading of the unretracted doctrine is treated as heresy rather than as a defensible interpretation of the very ambiguity leadership authored.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_adherents, excluded,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_adherents, payer).

% Entered marriages performed quietly under continuing authorization during the ambiguity window, then found themselves publicly disclaimed, legally unrecognized, and socially stigmatized once the institution needed to present a clean compliance narrative to Congress and the courts. Bear the cost of a policy that was never openly acknowledged, with no recourse to the institution that authorized their unions.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, wives_and_children_of_disavowed_marriages, payer,
    powerless, biographical, trapped, national).

% Received continued sanction for plural marriages performed outside direct U.S. jurisdictional reach (Mexico, Canada, at sea) during the years the public position claimed the practice had ended. Benefited from the doctrine/practice gap directly: it is the mechanism that let their marriages proceed with institutional blessing while the institution denied such blessing existed.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, post_manifesto_plural_families_in_claimed_jurisdictions, beneficiary,
    moderate, biographical, constrained, regional).

% Accept the Manifesto as the operative fact for purposes of statehood negotiations and anti-polygamy prosecution, while periodically investigating (Smoot hearings, 1904) whether the public declaration matches actual practice. Their leverage is what forces the doctrine/practice gap to eventually narrow, but they are not positioned to see the internal theological reasoning driving the gap's maintenance.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_and_state_authorities, observer,
    institutional, biographical, analytical, national).

% Issues the 1904 'Second Manifesto' under Joseph F. Smith, converting the ambiguous suspension into an enforced excommunication policy for continued plural marriage. Closes the gap this constraint describes by making practice suspension absolute and punishable, while still not repudiating Section 132 as scripture — narrowing but not resolving the doctrine/practice split.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, second_manifesto_era_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, church_institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ambiguity coordinates the institution's need to satisfy two audiences simultaneously without an internal schism: it lets leadership claim full compliance to federal authorities pursuing statehood and prosecution, while telling committed believers that the underlying revealed principle has not been abandoned, only its manner of public practice — avoiding a stark choice between total capitulation (which would read as denying continuing revelation) and total defiance (which would forfeit statehood and provoke federal seizure of Church property).
% TRANSFER_FUNCTION: Moves institutional risk and doctrinal clarity away from leadership and onto the general membership and fundamentalist adherents: the cost of the ambiguity (confusion, later betrayal, excommunication risk for those who read Section 132 literally) is paid by people who trusted either the public statement or the preserved doctrine at face value, while the institution retains the flexibility to authorize or deny marriages case by case, in jurisdictions of its choosing, without ever having to state a single coherent rule.
% ABSENT_VOICES: Fundamentalist adherents who continued the practice on the premise that Section 132 remained binding were never given a hearing on the doctrinal merits of their position — they were processed as a discipline problem, not a theological disagreement, even though the ambiguity that produced their reading was authored by the institution itself. Wives of the quietly-sanctioned post-Manifesto marriages had no forum in which their marriages' authorization could be acknowledged once it became politically costly to admit.
% DISAPPEARANCE_RATIONALE: If the doctrine/practice gap had not existed — if Section 132 had been formally revoked in 1890 alongside the public declaration, or if practice had continued openly and uniformly — the Church would have faced either an unambiguous schism (fundamentalists breaking immediately on record of formal revocation) or continued federal confrontation (open practice inviting renewed prosecution and property seizure). The gap is precisely what allowed the institution to negotiate statehood while retaining a flexible internal position; removing it collapses the negotiating space that produced the 1896 Utah statehood outcome.
% FOUNDING_PROBLEM: The Church faced likely dissolution as a corporate and property-holding entity under the Edmunds-Tucker Act, with federal seizure already underway and Utah statehood contingent on ending polygamy. Some form of accommodation with federal authority was existentially necessary.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the period (D. Michael Quinn's documentation of post-Manifesto marriages; the 1904-1907 Smoot hearing record itself, compiled by a hostile Senate committee) attest from outside the Church's own leadership that the federal legal threat which justified the original ambiguity was substantially resolved by Utah statehood in 1896, yet the doctrine/practice gap and selective enforcement persisted for over a decade afterward — corroboration exists outside the institution, though the Church's own later position (the 1904 Second Manifesto) implicitly concedes the same point by tightening enforcement rather than resolving the underlying doctrinal question.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising through the 1890s-1904 (0.55 to 0.78) because the ambiguity is not a passive residue but an actively exploited resource: it lets leadership authorize marriages case-by-case while denying a policy exists, extracting institutional flexibility at the direct cost of clarity for ordinary members and legal/social standing for those in the quietly-authorized marriages. Theater ratio climbs in parallel (0.40 to 0.65) as the public performance of compliance increasingly diverges from the private administration of exceptions — the Manifesto becomes more a performance for external audiences the longer clandestine authorization continues. Suppression spikes sharply at 1904 (0.75) coinciding with the Second Manifesto's enforced excommunication policy, which was a suppression-intensification response to the Smoot hearings exposing the gap, not a suppression response to failure of persuasion. The interval closes with all three metrics declining slightly (1907) as the gap narrows post-Smoot, though it does not close entirely — Section 132 is still not repudiated.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the gap looks like prudent stewardship: preserving core doctrine while managing an existential external threat. From the general membership and fundamentalist seats, the identical structure looks like an unacknowledged betrayal — a policy that was never stated plainly enough to be trusted or resisted on its actual terms. The engine should compute these as diverging per-seat classifications from the same structural facts; the ambiguity is exactly what makes both readings simultaneously available to different parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institutional leadership sits at the beneficiary end: it collects the flexibility itself, choosing which behaviors to sanction and which to punish without ever stating a uniform rule, and its exit options are effectively arbitrage — it can appeal to whichever face of the ambiguity best serves a given audience. General membership and fundamentalist adherents sit at the target end: they must act on an interpretation of an ambiguous signal, and whichever interpretation they choose (trust the public statement, or trust the preserved doctrine) can be later used against them. The wives and children of quietly-sanctioned marriages are especially trapped — their entire legal and social status depends on an authorization the institution will not admit having given once it becomes costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential federal threat to the Church's corporate survival) was substantially resolved by Utah statehood in 1896, yet the doctrine/practice gap and its selective enforcement persisted and even intensified through 1904 — precisely the mandatrophy pattern: the mechanism outlived the emergency that justified it and continued to be administered because it had become a source of institutional flexibility in its own right, not because the federal threat still required it. Classifying this as tangled_rope rather than pure snare preserves the genuine coordination function (avoiding total schism, negotiating survival) the ambiguity performed in 1890, while still registering the asymmetric extraction that followed once the emergency passed and the gap became a tool of dual-track legitimation rather than crisis management.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gap_intentional_vs_emergent,
    'Was the doctrine/practice gap a deliberately designed governance tool, or an emergent consequence of two genuinely separate processes (a political declaration and an unrevoked scripture) that were never reconciled because no single actor had authority to reconcile them?',
    'Archival evidence of First Presidency deliberations regarding the wording of the 1890 Manifesto and whether Section 132''s canonical status was explicitly discussed and left unresolved by design versus by oversight or disagreement among the Quorum.',
    'If deliberately designed, the tangled_rope classification is strongly supported (the ambiguity is an authored extraction mechanism). If emergent from institutional inability to reconcile competing claims, the constraint may better resemble an unintentional piton — a gap nobody chose to maintain but that nonetheless produced extraction through inertia rather than design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gap_intentional_vs_emergent, empirical, 'Whether the practice/doctrine gap was authored intentionally or emerged from unresolved institutional authority.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the practice_doctrine_gap reading itself a separate structural claim from the endogenous_reinterpretation_reading and exogenous_override_reading, or is it simply a description of how those two readings'' failure to fully displace one another looked from outside?',
    'Compare whether beneficiary/victim sets and extraction mechanisms differ substantively across the three readings (they do here: this reading names institutional flexibility itself as the extracted good, distinct from either the revelation-authenticity question or the pure-coercion question) — sustained structural distinctness across readings, rather than convergent language, would confirm this is a genuine third constraint, not a restatement.',
    'If the three readings collapse into restatements of one underlying fact pattern, the network of three sibling constraints should be merged; if they remain structurally distinct (different ε, different victim/beneficiary sets, different classification-relevant mechanisms as authored here), the decomposition is warranted per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the gap reading is a structurally distinct constraint or a restatement of the other two kernel readings.').

omega_variable(
    institutional_survival_as_beneficiary_category,
    'Is ''institutional survival'' a genuine beneficiary in the classification sense, or is it more accurately a vindicated proposition (the Church''s continued corporate existence) that itself has no distinct agent collecting rents, with the actual beneficiaries being the specific leadership figures and post-Manifesto plural families who obtained concrete benefit from the ambiguity?',
    'Distinguish abstract institutional continuity (which collects nothing directly) from the concrete decision-making authority (First Presidency and Apostles who administered exceptions) and the specific families who received sanctioned marriages — the schema requires beneficiaries to be real actors, not propositions.',
    'This story lists church_institutional_leadership and post_manifesto_plural_families_in_claimed_jurisdictions as the concrete beneficiaries rather than ''institutional survival'' as an abstract entity, per the schema''s requirement that beneficiaries be real actors; the vindicated_propositions field carries the doctrinal claims (continuing revelation, prophetic authority) that collect no rent themselves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_survival_as_beneficiary_category, conceptual, 'Whether institutional survival is a beneficiary or should be decomposed into concrete beneficiary actors plus vindicated propositions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1907).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(marr_tr_t1893, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1893, 0.48).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1896, 0.55).
narrative_ontology:measurement(marr_tr_t1899, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1899, 0.6).
narrative_ontology:measurement(marr_tr_t1902, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1902, 0.65).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1904, 0.62).
narrative_ontology:measurement(marr_tr_t1907, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1907, 0.58).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(marr_be_t1893, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1893, 0.62).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1896, 0.65).
narrative_ontology:measurement(marr_be_t1899, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1899, 0.7).
narrative_ontology:measurement(marr_be_t1902, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1902, 0.74).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1904, 0.78).
narrative_ontology:measurement(marr_be_t1907, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1907, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.4).
narrative_ontology:measurement(marr_su_t1893, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1893, 0.45).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1896, 0.48).
narrative_ontology:measurement(marr_su_t1899, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1899, 0.52).
narrative_ontology:measurement(marr_su_t1902, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1902, 0.58).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1904, 0.75).
narrative_ontology:measurement(marr_su_t1907, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1907, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__practice_doctrine_gap, 0.1).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the marriage_commitment_reversal kernel, decomposed per the ε-invariance principle because each reading has a structurally distinct beneficiary/victim set and extraction mechanism. exogenous_override_reading isolates the federal-coercion causal claim (extraction: none from the reversal itself, since it is framed as capitulation to external force with doctrine unchanged). endogenous_reinterpretation_reading isolates the revelation-authenticity claim (extraction: contested, turning on whether the 1890 vision was genuine continuing revelation or post-hoc rationalization). This reading (practice_doctrine_gap) isolates the ambiguity itself as an extractive institutional mechanism, independent of which causal story about the reversal's origin is correct — it has the highest authored extractiveness of the three because it is the only reading that identifies a concrete, actively-administered mechanism (selective enforcement, dual-track legitimation, ~200 marriages in claimed-legal jurisdictions) rather than a single-event causal claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
