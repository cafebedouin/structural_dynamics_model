% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: Exogenous Override Reading: Federal Coercive Suspension of Covenant Plural Marriage (1862-1896)
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   This story instantiates the exogenous-override reading of the
 *   marriage_commitment_reversal kernel: between the Morrill Anti-Bigamy Act
 *   (1862) and Utah statehood (1896), the federal government escalated from
 *   nominal statute through criminal prosecution, disfranchisement,
 *   disincorporation, and threatened total escheat of church property until
 *   the First Presidency publicly suspended sanctioned plural marriage in the
 *   October 1890 Manifesto - while the governing revelation, Doctrine and
 *   Covenants Section 132 (canonized 1876, commanding plural marriage as the
 *   condition of highest exaltation), remained canonically untouched. On this
 *   reading the reversal was external compulsion, not internal doctrinal
 *   revision: the practice stopped because continuation had been made
 *   impossible, and the principle survived intact beneath the compliance. The
 *   constraint under measurement is that coercive arrangement itself - the
 *   standing federal regime over LDS marital practice - assessed by this
 *   reading's own lights: it coordinates a national marriage standard and a
 *   sovereignty settlement, and it transfers autonomy, property, liberty, and
 *   political rights from the Latter-day Saint community to the federal state
 *   and its coalition. KEY AGENTS (by structural relationship): see
 *   key_agents. The claim/metric division is deliberate: the type claim is
 *   authored from structure, the metrics from operation; the engine computes
 *   per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - federal_territorial_administration: Primary agenda-setter (institutional/arbitrage) - sets enforcement terms, collects sovereignty and exposed assets
 *   - - national_republican_coalition: Pure beneficiary (powerful/mobile) - collects electoral rents at negligible cost
 *   - - lds_first_presidency: Primary payer with secondary administrative seat (organized/identity_locked) - absorbs existential coercion, then administers compliance
 *   - - lds_plural_husbands: Direct payer (powerless/trapped) - imprisonment or underground exile
 *   - - plural_marriage_wives: Direct payer, structurally excluded (powerless/trapped) - sharpest personal costs, no negotiating seat
 *   - - lds_rank_and_file_members: Diffuse payer (organized/identity_locked) - disfranchised, pressured; collective resistance capacity
 *   - - utah_woman_suffrage_community: Collateral payer (moderate/constrained) - loses franchise to a constraint aimed elsewhere
 *   - - mormon_mexico_colonists: Payer exercising costly partial exit (moderate/mobile)
 *   - - constitutional_historians: Analytical observer (analytical/analytical) - sees full structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.71).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.45).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "Exogenous Override Reading: Federal Coercive Suspension of Covenant Plural Marriage (1862-1896)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious institutional history / commitment systems / political theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, 'bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1').
narrative_ontology:cs_kernel_codification('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', fixed_text).
narrative_ontology:cs_authority_grounding('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', lineage).
narrative_ontology:cs_interpretation_layer_present('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1').
narrative_ontology:cs_reading_relation('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', foundational, civil_coercion_cannot_annul_divine_covenant).
narrative_ontology:cs_axiom_status(civil_coercion_cannot_annul_divine_covenant, holdable).
narrative_ontology:cs_axiom_grounding('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', civil_coercion_cannot_annul_divine_covenant, theological).
narrative_ontology:cs_axiom('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', foundational, manifesto_effected_compliance_without_doctrinal_revision).
narrative_ontology:cs_axiom_status(manifesto_effected_compliance_without_doctrinal_revision, holdable).
narrative_ontology:cs_axiom_grounding('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', manifesto_effected_compliance_without_doctrinal_revision, empirically_contingent).
narrative_ontology:cs_reference_frame('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', covenant_binding_independent_of_civil_power).
narrative_ontology:cs_drift_state('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', manifesto_era_post_statehood, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('bcbe21a1-6cd0-4f59-9f4b-d9a8c93f82e1', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_administration).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, national_republican_coalition).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_first_presidency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_plural_husbands).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_wives).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, utah_woman_suffrage_community).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, mormon_mexico_colonists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress, the Department of Justice, and appointed territorial governors and judges prosecute plural marriage as a condition of Utah's incorporation: the Morrill, Poland, Edmunds, and Edmunds-Tucker acts escalate from unenforced statute to unlawful-cohabitation trials, disfranchisement machinery run by election commissioners, disincorporation of the church, and escheat of its corporate property. The administration sets the terms both of pressure and of retreat (amnesty, statehood), and collects the consolidated sovereignty, the exposed assets, and the precedent that civil law governs family formation everywhere in the republic.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Party organizations and allied moral-reform societies made eradication of the 'twin relic' a defining platform plank from 1856 onward. The campaign supplied a mobilizing grievance that outlasted slavery's removal; officeholders rode it to elections and filled territorial posts through it as patronage. Their exposure to the constraint's costs was negligible, and once statehood changed the subject, exiting the position was costless.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, national_republican_coalition, beneficiary,
    powerful, biographical, mobile, national).

% Presidents Young, Taylor, and Woodruff administered the covenant practice internally - calling plural marriages, disciplining dissent, defending the principle in public epistles - while absorbing escalating external costs: exile underground, imprisonment of counselors and apostles, threatened total property forfeiture and organizational extinction. After October 1890 the same office administers compliance with the imposed suspension, issuing and policing the Manifesto, without revising the canonical text that commands the practice. Exit would mean renouncing the covenant itself; the office's authority is constituted as custodianship of the sealed revelation.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_first_presidency, payer,
    organized, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_first_presidency, agenda_setter).

% Men sealed to plural wives faced indictment under the Edmunds Act's unlawful-cohabitation charge - drawn so that a lawful first marriage plus any additional sealing was prosecutable - with penitentiary sentences of months to years. Hiding meant flight from livelihood and family along the Mormon 'Underground Railroad'; surrender meant prison and the test oath's further penalties. The only legally clean exit, abandoning wives and children, was religiously and morally unavailable to them.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_plural_husbands, payer,
    powerless, biographical, trapped, regional).

% Women in plural marriages bore the sharpest personal costs: husbands imprisoned or in flight, households sustained alone, social stigma, and after 1887 loss of the Utah vote they had held since 1870. Neither Congress's hearings nor the First Presidency's councils consulted them; their consent was irrelevant to the practice's maintenance and to its suspension alike. Some testified under prosecutors' questioning; none sat where the Manifesto was drafted.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_wives, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_wives, excluded).

% Ordinary members bore diffuse costs: the 1887 test oath disqualified believers from voting and office, economic boycott pressures mounted, missions were disrupted, and leaders lived in hiding. Church organization concentrated their resistance - block voting, unified noncompliance, funding for fugitives - which is precisely what made them legible to Washington as a polity needing the test oath. Leaving the faith meant severing kinship, community, and perceived salvation; the constraint fused with covenant identity rather than merely pricing behavior.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_rank_and_file_members, payer,
    organized, generational, identity_locked, regional).

% Utah women, Mormon and non-Mormon alike, voted from 1870 until the Edmunds-Tucker Act stripped the franchise from all of them in 1887 as an anti-polygamy measure. Non-LDS women and national suffrage allies lost a working franchise to a constraint aimed elsewhere - collateral extraction sweeping bystanders into the paying seats. Their recourse was petitioning Congress for restoration, which arrived only with statehood in 1896.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, utah_woman_suffrage_community, payer,
    moderate, generational, constrained, regional).

% From 1885 the church sponsored colonies in Chihuahua and Sonora partly as an exit channel where plural marriage remained lawful. Colonists abandoned developed homes and congregations for frontier hardship; the channel was partial and costly, available to those with means or called labor and closed to the poor, and it kept the collapse of alternatives short of totality while the main body complied.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, mormon_mexico_colonists, payer,
    moderate, generational, mobile, continental).

% Scholars of law and religion reconstruct the sequence from statutes, court records, diaries, and the church's own publications, adjudicating between causal accounts of the 1890 reversal. They collect no rents and bear none of its costs; their seat is the vantage from which the preserved-canon-over-suspended-practice structure is visible as a stable configuration rather than a transient embarrassment.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_administration).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The regime coordinated a single marriage-law standard across a territory seeking statehood and adjudicated the collision between an ecclesiastical jurisdiction claiming authority over family formation and a civil jurisdiction claiming monopoly on marriage regulation. Its enforcement machinery also standardized territorial voter qualification through the test oath and commission-administered elections.
% TRANSFER_FUNCTION: Moved institutional autonomy, corporate property (church assets exposed to escheat under Edmunds-Tucker), personal liberty (hundreds of imprisonments for unlawful cohabitation), and political rights (the franchise stripped from all Utah women in 1887, church-affiliated men disqualified by test oaths) from the Latter-day Saint community to the federal state and its governing coalition.
% ABSENT_VOICES: The women inside plural marriages had no seat in any negotiation - not in congressional hearings, not in the councils that produced the Manifesto; their interests were spoken for by both sides. Rank-and-file believers learned the outcome after the fact. Later fundamentalist dissenters, whose objection (suspension without revelation or revision) defines one sibling reading, had no voice in the conversation at all.
% DISAPPEARANCE_RATIONALE: Church leadership had affirmed for four decades that plural marriage was non-negotiable: Taylor went underground in 1885 rather than submit, and his 1886 revelation reaffirmed the principle. Had the coercive regime vanished overnight in, say, 1889, the practice would have continued openly, Utah statehood would have stayed indefinitely deferred, the national monogamous standard would have gone unasserted in the territory, and Section 132 would have remained lived law rather than suspended canon.
% FOUNDING_PROBLEM: Built to solve the collision between a territory-governing church whose revealed law mandated plural marriage and a federal republic whose law and self-definition required monogamy - concretely, how to incorporate Utah while breaking the ecclesiastical hold on its politics and family structure.
% FOUNDING_PROBLEM_CORROBORATION: Federal-side sources (congressional debate, Reynolds v. United States, the Late Corporation opinion) corroborate the problem-as-stated and declare it resolved by submission - but those are instruments of the benefiting party, not neutral witnesses. Genuinely external corroboration of the problem's contested status comes from constitutional and religious historians working outside both camps, and from the fundamentalist schism record together with the church's own retention of Section 132 in canon, which jointly attest that the theological question was suspended rather than settled. No party outside the dispute attests it simply dead.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71 (end-state) because what moved was not payment-for-service but self-governance itself: the church surrendered the right to define its own family law, saw corporate property exposed to escheat, absorbed hundreds of imprisonments, and regained ordinary civic standing only on the state's terms - with Section 132 unchanged, so nothing internal was traded for the relief. The trajectory (0.12 rising to 0.86 at capitulation, decaying to 0.71 after amnesty and statehood) is a coercion ratchet followed by post-settlement decay, not a cycle; no oscillation mechanism operates, and the base_properties scalars describe the end-state. Suppression (0.45 end-state, peaking at 0.90 in 1890) is structural throughout - statutes, penitentiaries, election commissions, disincorporation orders - with a negligible internalized component: acquiescence followed material impossibility, not belief-change, which is precisely this reading's thesis. Theater_ratio jumps at 1890 (0.56): the Manifesto is public compliance performance over an unrevised canon, with official observance and continued private sealings diverging until the 1904 Second Manifesto (outside this interval), then settles to 0.38 as formal observance is maintained over the preserved principle. Accessibility_collapse 0.62: alternatives narrowed to comply, incarcerate, or flee, with the Mexico-colony channel keeping collapse short of totality. Resistance 0.25 reflects the exhausted end-state: decades of block voting, fugitive networks, and Taylor's 1886 reaffirmation collapsed into accommodation by 1896, leaving a small fundamentalist remnant. Identity-lock mechanics: the First Presidency's exit is identity_locked because the office's authority is constituted as custodianship of the sealed covenant - abandoning Section 132 to appease the state would dissolve the very authority the office exists to exercise; rank-and-file lock is relational and communal (kinship, salvation, and belonging fused with compliance). Were the covenant frame to break - members concluding the command was genuinely rescinded - locks would loosen toward constrained, effective extraction would fall, and the account would migrate toward the endogenous sibling reading. Claim/metric independence: claimed_type tangled_rope is authored from structure (a genuine coordination settlement - one marriage law, an adjudicated church-state boundary, a statehood pathway - carrying strongly asymmetric extraction under continuous enforcement); the metrics are authored from operation. Nothing above is tuned toward a predicted engine verdict.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the federal seat the same statutes are lawful coordination: enforcing the republican form, protecting a territorial electorate from what Washington read as theocratic control, conditioning admission on conformity - a rope-shaped world. From the payer seats the identical machinery is existential extraction: husbands imprisoned, women disfranchised who had voted since 1870, a corporation dissolved, a covenant suspended by bayonet rather than by revelation - a snare-shaped world. The plural-wives seat is doubly displaced, harmed by the practice's enemies and its defenders alike and consulted by neither. The historian seat holds both shapes at once and treats the gap between them as the datum. Same-power differentiation: two organized religious bodies of comparable standing experienced the era oppositely because this constraint's enforcement keyed on marital practice and corporate form, not on religiosity generally - constraint-specific factors, not global power, drive the divergence. The engine computes all of this from power, exit, and directional data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: the federal territorial administration receives sovereignty consolidation plus assets pending escheat proceedings - d nearest the beneficiary end; the Republican coalition collects electoral rents at negligible personal cost - d near zero. Victim declarations map to the paying seats: first presidency, plural husbands, plural wives, rank-and-file members, and the disfranchised suffrage community all sit near the full-target end, amplified by trapped and identity_locked exit (locked targets sit nearer the full-target end than mobile ones). The Mexico colonists remain targets whose mobile exit moderates but does not invert d - their exit is costly relocation, not arbitrage-grade relief. Suppression is authored as a raw structural property and is scaled by nothing; only extractiveness is scaled, by directionality and scope, and the continental reach of the enforcement standard amplifies effective extraction at large scope. No directionality_overrides are used: the beneficiary/victim declarations plus exit options reproduce the true relationships without correction, and the first presidency's secondary administrative seat does not move it off the target end because what it administers post-1890 is the imposed suspension, not a constraint it authored.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is deliberately not declared. The regime's mandate - breaking ecclesiastical sovereignty sufficiently to admit Utah - completed with statehood, and enforcement then decayed by success (amnesty, eventual return of property, reversion to ordinary law). That is dissolution rather than atrophy: no inert shell persists under theatrical maintenance, so no piton signature applies, and the theater_ratio rise marks a compliance-performance phase rather than vestigial bureaucracy. The founding problem is authored contested rather than dead: federal seats attest resolution, while the retained canon and the fundamentalist schism attest a theological question suspended, never answered - the mismatch consumer reads contested x world_rearranges and correctly finds no zombie flag, since nothing inert persists; what persists is a live doctrinal fault line this reading expects to stay open. The tangled_rope classification prevents two symmetrical mislabels: the rope-mislabel (reformer historiography reading the campaign as pure civic hygiene, erasing the payer asymmetry) and the snare-mislabel (polemical historiography reading it as pure plunder, erasing the genuine coordination settlement the coercion purchased). Both functions ran through the same statutes; separating them is the classifier's work, not the polemicists'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_epsilon_authoring,
    'This file instantiates one reading (exogenous_override_reading) of the kernel marriage_commitment_reversal; epsilon is authored under that reading''s lights for the shared referent - the standing coercive arrangement of 1862-1896. Would the sibling readings author materially different epsilon for the same referent?',
    'Compile the sibling files (endogenous_reinterpretation_reading, practice_doctrine_gap) and compare epsilon over the identical referent; cross-reading divergence is the corpus-level measurement, not an error to repair.',
    'Locates the kernel dispute in the valuation of coercion versus revelation rather than in the arrangement itself; per-seat classifications differ across sibling files by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_epsilon_authoring, conceptual, 'Committer-frame bookkeeping: reading-indexed epsilon over a fixed referent.').

omega_variable(
    causal_locus_of_1890_reversal,
    'Was the operative cause of the October 1890 Manifesto federal coercion alone, divine revelation (Woodruff''s reported September vision), or a compound - and can the record distinguish conviction from capitulation?',
    'Archival convergence: Woodruff''s diaries and private correspondence, the enforcement timeline (Edmunds-Tucker passage, escheat rulings), contemporaneous apostolic testimony, and the documented private continuance of some sealings after 1890.',
    'A demonstrated revelatory locus shifts weight toward the endogenous sibling and lowers the measured extraction of autonomy; a coercion-only locus fixes this reading''s high-epsilon account and roots the doctrine-practice gap in external force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_locus_of_1890_reversal, empirical, 'The kernel''s central dispute: where the causal arrow of the reversal sits.').

omega_variable(
    protection_or_cover_beneficiary_structure,
    'Did the federal campaign confer any genuine benefit on women inside plural marriages (autonomy, exit from unwanted unions), or was protective rhetoric cover for sovereignty consolidation - that is, is the beneficiary set larger than the two state-side seats?',
    'Outcome analysis of plural wives'' circumstances after the Manifesto: property, mobility, access to divorce, and whether prosecution and disfranchisement improved or worsened their practical position; contemporary testimony of women both supporting and opposing the practice.',
    'Demonstrable protective benefit would add a genuine beneficiary seat and soften the extraction asymmetry this reading authors; pure cover confirms the current structure and indicts the protective framing as enforcement decoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_or_cover_beneficiary_structure, empirical, 'Contestable beneficiary structure beneath the protective rhetoric.').

omega_variable(
    section132_revision_latency,
    'Will the preserved principle (Section 132, unrevised in canon) ever receive formal doctrinal revision, and does the open file sustain latent revival movements (the early-twentieth-century fundamentalist schism and its modern successors)?',
    'Track canon-revision decisions, First Presidency statements, and formation rates of schismatic movements against the retained text.',
    'Permanent retention keeps the doctrine-practice gap structurally alive and this reading''s account predictive; formal revision would close the gap retroactively and recode the 1890 event as partial endogenous reinterpretation after all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section132_revision_latency, empirical, 'The unrevised canon as a live structural variable sustaining latent revival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1862, 1896).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mmcr_exog_tr_t1862, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1862, 0.05).
narrative_ontology:measurement(mmcr_exog_tr_t1874, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1874, 0.08).
narrative_ontology:measurement(mmcr_exog_tr_t1882, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1882, 0.14).
narrative_ontology:measurement(mmcr_exog_tr_t1887, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1887, 0.18).
narrative_ontology:measurement(mmcr_exog_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.56).
narrative_ontology:measurement(mmcr_exog_tr_t1893, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1893, 0.49).
narrative_ontology:measurement(mmcr_exog_tr_t1896, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1896, 0.38).

% Extraction over time
narrative_ontology:measurement(mmcr_exog_be_t1862, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1862, 0.12).
narrative_ontology:measurement(mmcr_exog_be_t1874, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1874, 0.24).
narrative_ontology:measurement(mmcr_exog_be_t1882, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1882, 0.46).
narrative_ontology:measurement(mmcr_exog_be_t1887, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1887, 0.72).
narrative_ontology:measurement(mmcr_exog_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.86).
narrative_ontology:measurement(mmcr_exog_be_t1893, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1893, 0.79).
narrative_ontology:measurement(mmcr_exog_be_t1896, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1896, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(mmcr_exog_su_t1862, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1862, 0.1).
narrative_ontology:measurement(mmcr_exog_su_t1874, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1874, 0.2).
narrative_ontology:measurement(mmcr_exog_su_t1882, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1882, 0.52).
narrative_ontology:measurement(mmcr_exog_su_t1887, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1887, 0.76).
narrative_ontology:measurement(mmcr_exog_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(mmcr_exog_su_t1893, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1893, 0.58).
narrative_ontology:measurement(mmcr_exog_su_t1896, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1896, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% The colloquial label 'the 1890 Manifesto ending plural marriage' decomposes into three structurally distinct readings of one kernel (marriage_commitment_reversal), per the epsilon-invariance principle: this file authors the exogenous-override reading (reversal by federal coercion, canon untouched); endogenous_reinterpretation_reading authors reversal-by-revelation; practice_doctrine_gap authors the persistent preserved-principle/suspended-practice structure. Each carries its own reading-indexed epsilon over the shared referent (OQ-26); values differ by reading, and that divergence is the measurement. Upstream, the federal-supremacy legal record feeds this reading; downstream, the override this reading describes creates the conditions the gap reading names. Family linkage runs through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
