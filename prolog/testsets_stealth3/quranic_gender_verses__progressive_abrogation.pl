% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Progressive-Abrogation Reading of the Gender-Specific Qur'anic Rulings
 *   domain: religious/legal-hermeneutic/gender
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the normative status of the Qur'an's gender-specific legal verses (4:11
 *   inheritance shares, 2:282 witness arithmetic, 4:34 household authority).
 *   Three readings compete: literal_hierarchical (timeless divine ordinance),
 *   contextual_egalitarian (historically situated steps requiring
 *   maqasid-guided reinterpretation), and THIS reading,
 *   progressive_abrogation: the verses are an incomplete trajectory, and
 *   later egalitarian principles (49:13 universal human dignity) supersede
 *   them via naskh. The constraint classified here is the standing
 *   arrangement those verses anchor - the classical fiqh enforcement regime -
 *   assessed through this reading's own lights. Epsilon's referent is that
 *   standing arrangement, never this reading's endorsed egalitarian
 *   alternative. Through this reading's lights the arrangement is maximally
 *   extractive: it compels compliance with rules the reading holds divinely
 *   superseded, under a permanence claim the reading's central premise
 *   refutes. KEY AGENTS (by structural relationship): -
 *   traditional_juridical_establishment: agenda-setter and principal
 *   beneficiary (institutional/identity_locked) - administers enforcement,
 *   collects interpretive authority - muslim_women_under_classical_rulings:
 *   primary target (powerless/trapped) - bear differential shares, testimony
 *   weighting, guardianship provisions - male_household_guardians: secondary
 *   beneficiary (moderate/constrained) - receive preferential shares and
 *   household authority - reformist_hermeneuts: dissenting advocates of this
 *   reading (organized/identity_locked) - bear sanction costs for advancing
 *   abrogation from inside the tradition - literal_identity_communities:
 *   identity-bound supporters (organized/identity_locked) - collect
 *   existential coherence; exposed to epistemic costs if the reading prevails
 *   - state_family_law_regulators: partial implementers
 *   (institutional/constrained) - some jurisdictions already run modified
 *   regimes - academic_quranic_studies: analytical observer
 *   (institutional/analytical) - attests historical situatedness from outside
 *   all confessional seats
 *
 * KEY AGENTS:
 *   - traditional_juridical_establishment: agenda_setter + beneficiary (institutional power, identity_locked exit, global scope) - runs the academies, fatwa councils, and court training through which the rulings operate; its authority is the rent the arrangement pays it
 *   - muslim_women_under_classical_rulings: payer (powerless, trapped, global scope) - bear the material and standing costs of the differential; individual exit carries family rupture and communal expulsion
 *   - male_household_guardians: beneficiary (moderate, constrained, global scope) - receive preferential shares and domestic authority without administering the system
 *   - reformist_hermeneuts: payer + prospective beneficiary (organized, identity_locked, global scope) - advance this very reading; pay in stalled careers and deviance charges; cannot exit the tradition their faith identity constitutes
 *   - literal_identity_communities: beneficiary (organized, identity_locked, regional scope) - hold existential coherence from the arrangement's permanence claim; the population this reading's success would most disrupt
 *   - state_family_law_regulators: observer (institutional, constrained, national scope) - codify variants; several jurisdictions demonstrate state-level departure is feasible
 *   - academic_quranic_studies: observer (institutional, analytical, global scope) - supply the chronological and philological evidence on which this reading's engine lives or dies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.88).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.78).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, snare).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Progressive-Abrogation Reading of the Gender-Specific Qur'anic Rulings").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/legal-hermeneutic/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '57ffcc3c-4700-4d13-8b5c-d915456332d4').
narrative_ontology:cs_kernel_codification('57ffcc3c-4700-4d13-8b5c-d915456332d4', fixed_text).
narrative_ontology:cs_authority_grounding('57ffcc3c-4700-4d13-8b5c-d915456332d4', lineage).
narrative_ontology:cs_interpretation_layer_present('57ffcc3c-4700-4d13-8b5c-d915456332d4').
narrative_ontology:cs_reading_relation('57ffcc3c-4700-4d13-8b5c-d915456332d4', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('57ffcc3c-4700-4d13-8b5c-d915456332d4', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('57ffcc3c-4700-4d13-8b5c-d915456332d4', foundational, later_revelation_supersedes_earlier_gender_rulings).
narrative_ontology:cs_axiom_status(later_revelation_supersedes_earlier_gender_rulings, holdable).
narrative_ontology:cs_axiom_grounding('57ffcc3c-4700-4d13-8b5c-d915456332d4', later_revelation_supersedes_earlier_gender_rulings, empirically_contingent).
narrative_ontology:cs_axiom('57ffcc3c-4700-4d13-8b5c-d915456332d4', foundational, universal_dignity_principle_normatively_supreme).
narrative_ontology:cs_axiom_status(universal_dignity_principle_normatively_supreme, holdable).
narrative_ontology:cs_axiom_grounding('57ffcc3c-4700-4d13-8b5c-d915456332d4', universal_dignity_principle_normatively_supreme, deontological).
narrative_ontology:cs_reference_frame('57ffcc3c-4700-4d13-8b5c-d915456332d4', completed_ethical_trajectory_toward_parity).
narrative_ontology:cs_drift_state('57ffcc3c-4700-4d13-8b5c-d915456332d4', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('57ffcc3c-4700-4d13-8b5c-d915456332d4', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, traditional_juridical_establishment).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, male_household_guardians).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, literal_identity_communities).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, muslim_women_under_classical_rulings).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, reformist_hermeneuts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, reformist_hermeneuts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the academies, fatwa councils, and court-training pipelines through which the gender-specific rulings (4:11 inheritance shares, 2:282 witness arithmetic, 4:34 household authority) are taught as permanently binding divine law. Issues judgments, credentials jurists, and classifies rival readings as deviation. Its standing, funding, and adjudicative role flow from being the arbiter of these rulings; renouncing them would dissolve the institution's claim to transmit an intact revelation, so the option is not seriously entertained from inside.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_juridical_establishment, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, traditional_juridical_establishment, beneficiary).

% Live under the rulings where they are operative: smaller fixed shares in many inheritance configurations, witness-weighting in some classical applications, and household-authority provisions. Individual exit - declining the shares, refusing the arbitration, or leaving the tradition - carries family rupture, community expulsion, and in some jurisdictions legal jeopardy, so most comply within the system while collective reform proceeds slowly or abroad. Coalition potential exists (reformist networks, feminist jurisprudence circles) but is unevenly reachable from inside affected households.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, muslim_women_under_classical_rulings, payer,
    powerless, biographical, trapped, global).

% Husbands, fathers, sons, and brothers who receive the preferential shares and hold the domestic authority the provisions assign. Most regard the distribution as divinely fixed rather than personally chosen; their material position and household standing depend on its continuation, though few of them administer, teach, or defend the system institutionally. Were the distribution changed, they would lose relative advantage without existential harm - their stake is advantage, not survival.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, male_household_guardians, beneficiary,
    moderate, generational, constrained, global).

% Scholars and jurists inside or adjacent to the traditional academies who argue from the tradition's own tools - naskh methodology, the chronology of revelation, 49:13 - that the gender-specific rulings are superseded. They publish, teach, and petition official bodies; careers stall, fatwa councils exclude them, and some face innovation or deviance proceedings. Leaving the tradition entirely is closed to them by their own faith commitment, so they absorb the costs of advocating from within and would collect interpretive standing if their reading prevailed.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, reformist_hermeneuts, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, reformist_hermeneuts, beneficiary).

% Lay and clerical communities whose religious identity is bound to reading the rulings as timeless divine ordinance. The arrangement gives them existential coherence: scripture as fixed, complete, and authoritative. They defend it in public debate and raise children into it. A successful abrogation reading would unsettle the foundation of that identity rather than their material position, which is why their resistance is fiercer than the material beneficiaries' and why they are the population most exposed to disruption if this reading prevails.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, literal_identity_communities, beneficiary,
    organized, generational, identity_locked, regional).

% Legislatures and courts in Muslim-majority and Muslim-minority states that decide how much of the classical distribution to codify. Several jurisdictions already operate equal-inheritance or reformed family codes; others enforce the classical shares verbatim. Their record demonstrates that state-level departure is feasible, purchased at the price of recurring legitimacy conflict with the establishment and, in some cases, of exporting the dispute to diaspora communities.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, state_family_law_regulators, observer,
    institutional, generational, constrained, national).

% University-based historians and philologists of early Islam who date the verses, reconstruct the occasions of revelation, and document the seventh-century problems the rulings addressed. They hold no stake in the arrangement's continuation and corroborate or challenge this reading's chronological premises from outside every confessional seat - including the finding that the egalitarian proof-text may not postdate the restrictive rulings, on which this reading's engine depends.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, academic_quranic_studies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, traditional_juridical_establishment).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, divinely-sanctioned template for family property division, witness weighting, and marital governance across a geographically vast and legally plural civilization - solving predictability, dispute-resolution, and widow/orphan-protection problems without requiring centralized enforcement infrastructure.
% TRANSFER_FUNCTION: Moves property shares, adjudicative standing, and household authority from women to male kin under the operative rulings, and moves interpretive monopoly over the relevant verses to the scholarly establishment that certifies their meaning.
% ABSENT_VOICES: Muslim women subject to the rulings are largely absent from the forums that set them - historically excluded from classical juristic deliberation and underrepresented in contemporary fatwa councils. Reformist hermeneuts are excluded from official bodies precisely for the positions this reading advances. The people the verses govern speak about the rules mostly in venues the rules' administrators do not control.
% DISAPPEARANCE_RATIONALE: If the enforcement arrangement vanished overnight, inheritance flows, testimony practice, and household-authority patterns would reorganize immediately - the record of jurisdictions that already substituted reformed family codes shows the rearrangement is concretely available, and diaspora communities split along the resulting fault lines today.
% FOUNDING_PROBLEM: Seventh-century Arabian family regulation: securing widows' and orphans' property claims amid tribal conflict, ordering households in a stratified patriarchal society, and giving a new political community authoritative, workable family legislation.
% FOUNDING_PROBLEM_CORROBORATION: Contested by design. The traditional establishment attests liveness from inside the beneficiary set (divine law does not expire). Outside corroboration supports the transformed-or-dead reading: academic Quranic historians document the rulings' seventh-century occasional specificity; the civil codes of numerous Muslim-majority and Muslim-minority jurisdictions already perform the founding functions (widow security, orphan property) through other mechanisms; Muslim feminist jurists attest that the parity question is now governed elsewhere. No party outside the beneficiary set attests that the original problem persists in its original form.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.88 is authored through this reading's own lights over the standing arrangement: a regime compelling obedience to rules the reading holds abrogated, justified by a divine-permanence claim the reading's core premise denies, is close to the extraction ceiling. Suppression 0.78 is a raw structural property (unscaled by power or scope): enforcement runs through institutional credentialing, innovation and apostasy-adjacent charges, and family-level sanction; an internalized component is suspected and routed to an omega rather than folded into the scalar. Theater 0.38: the jurisprudence is functionally real (courts, fatwa, teaching), but a growing share of activity is apologetic performance defending the rulings against reform critique rather than adjudication. Accessibility_collapse 0.62: egalitarian alternatives are articulated and known, but for insiders they collapse behind exit costs approaching apostasy stakes. Resistance 0.68: sustained reformist scholarship, Muslim feminist jurisprudence, and state-level family-code departures meet the arrangement head-on. Claim and metrics are independent authored facts: claimed_type snare is my structural judgment from this reading's seat (the coordination story - divine timelessness - is precisely what the reading refutes, leaving coercion-sustained extraction with identifiable victims); the metrics describe observed operation. The engine computes per-seat types from the structural data; divergence between seats is the measurement, not an error to reconcile.
 *   
 *   Temporal grid: interval 0-70 maps approximately to 1955-2025. One shared grid for all three series (points 0,10,20,30,40,50,60,70). Base_extractiveness dips at t=10 (statist modernization briefly softened enforcement, e.g. reformed family codes) then climbs monotonically as revivalism rebuilt enforcement AND as egalitarian alternatives proliferated - continuing to enforce contested rules in front of known alternatives grows more extractive through this reading's lights. Suppression_requirement traces the enforcement-capacity arc: mid-century relaxation (0.54 at t=10), revivalist rebuild (0.72 by t=40), hardening plateau since. Theater_ratio rises steadily as public defense of the rulings shifted from juristic work to apologetic performance. All endpoints match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the traditional_juridical_establishment seat, the arrangement is continuous divine order it stewards - beneficiary directionality damps effective extraction toward subsidy, and the seat experiences reform as attack, not extraction. From the muslim_women seat, the same structure computes as coerced differential with no exit - full-target directionality, maximal chi. From the reformist_hermeneuts seat, the arrangement extracts twice: once in the rulings themselves, once in the sanction fell on anyone who says aloud what this reading says. From the literal_identity_communities seat, the arrangement delivers existential coherence and its defense is identity maintenance. The engine owns these computations; this story supplies the structural asymmetries (trapped vs identity_locked vs constrained exit; powerless vs institutional power) that make the divergence real rather than rhetorical.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: the establishment (agenda-setter collecting authority rents), male guardians (material preferential shares), and literal-identity communities (coherence goods) all sit toward the subsidized end, with the establishment's identity_locked exit anchoring it further from target position despite its enforcement labor. Victim declarations drive high d: women under the rulings sit nearest full-target - powerless power atom and trapped exit amplify; reformist hermeneuts carry elevated d as payers-by-dissent, sanctioned for advocating the very reading this story instantiates. No directionality_overrides are used: the beneficiary/victim declarations plus exit atoms already separate the seats, and the one subtle case (literal communities, who would pay under this reading's success but benefit under the standing arrangement) is correctly resolved by referencing the arrangement, which is the epsilon referent. Receipt surface: the arrangement's gains land in two sinks - authority rents at the establishment, property shares at the guardians. gain_flow names traditional_juridical_establishment because it is the seat that administers the rules AND captures the meta-rent (credentialing, adjudicative monopoly, institutional continuity) that sustains the whole structure; guardian receipts are distributed pass-through of rules the establishment maintains. fixing_cost is prohibitive for the seat that could fix it: the establishment could dissolve the arrangement by accepting the abrogation reading, but doing so dissolves the fixer's own authority foundation - the cost of fixing exceeds anything the fixer bears from the status quo.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. First, the false-summit mislabel: reading the arrangement as untouchable divine fixity (mountain-shaped) when it has named beneficiaries, human enforcement machinery, and mounting resistance - declaring beneficiaries and authoring honest metrics keeps the FSM question open rather than letting naturality language immunize it. Second, the costless-liberation mislabel: treating this reading's adoption as a pure Pareto move. The omegas hold the counter-ledger open - epistemic violence exposure for literal-identity communities, prohibitive adoption costs for scholars inside the institutions, and the chronological fragility of the abrogation arrow itself. On mandatrophy proper: the founding problem (workable family legislation for a 7th-century community - widow security, orphan property, household order) has been substantially transformed; civil law now performs many of those functions across the Muslim world, yet the gender-specific provisions persist where enforced. Hence founding_problem_status contested rather than dead: the establishment attests liveness from inside the beneficiary set, while academic historians and reformed-jurisdiction practice corroborate transformation from outside it. Because status is contested rather than dead, the status-x-verdict mismatch flag does not fire - but the contested status itself routes scrutiny through the kernel family, where the sibling readings carry the same referent with different epsilon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading (progressive_abrogation) of the kernel quranic_gender_verses; how would the sibling readings (literal_hierarchical, contextual_egalitarian) restructure the beneficiary/victim sets and epsilon if instantiated instead?',
    'Generate the sibling stories as separate constraint files over the same referent; compare computed per-seat classifications and epsilon across the family.',
    'Classification is reading-indexed: literal_hierarchical would author lower epsilon over the identical referent (the divine order is accepted as legitimate); contextual_egalitarian intermediate. Only cross-reading comparison settles the kernel''s structure; this file alone cannot.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: this story is one reading of a three-reading kernel; sibling deltas are structural, not noise.').

omega_variable(
    naskh_applicability_to_gender_verses,
    'Does the classical naskh methodology legitimately extend to these verses - is there authentic evidence that later revelation abrogated 4:11, 2:282, or 4:34?',
    'Philological audit of abrogation claims: matn-to-matn contradiction criteria, asbab al-nuzul chains, and the classical usul condition that abrogation require a later ruling on the same subject with reliable transmission.',
    'If the naskh evidence fails, this reading collapses toward the contextual_egalitarian sibling; if it holds, the standing arrangement enforces divinely superseded law and the high-extraction assessment strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_applicability_to_gender_verses, empirical, 'Whether the abrogation engine of this reading satisfies classical usul al-fiqh conditions.').

omega_variable(
    revelation_chronology_direction,
    'Is the egalitarian proof-text (49:13) actually later than the restrictive rulings (4:11, 2:282, 4:34) - the direction the abrogation claim requires?',
    'Isnad-critical and stylistic dating of the surahs. Most chronologies place 49:13 in the middle Medinan period while surah 2''s debt verse and much of surah 4 are late Medinan; if the restrictive verses postdate the egalitarian principle, the abrogation arrow reverses or fails.',
    'The reading''s entire engine depends on the arrow''s direction. A failed direction removes its distinguishing premise relative to contextual_egalitarian and leaves the standing arrangement''s defenders holding the stronger chronological position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revelation_chronology_direction, empirical, 'Chronological fragility of the later-supersedes-earlier premise.').

omega_variable(
    epistemic_violence_exposure,
    'If this reading prevailed, what identity-disruption costs fall on communities whose religious identity is bound to the literal reading, and do those costs belong in the arrangement''s total ledger?',
    'Track identity-coherence outcomes where egalitarian codification succeeded (societies with reformed family codes): disaffiliation rates, schism formation, fundamentalist backlash intensity across generations.',
    'Substantial epistemic costs would mean the reversal is not a pure improvement - the constraint family''s accounting must include this reading''s own casualties, tempering straightforward liberation narratives and feeding back into per-seat classification for literal_identity_communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_violence_exposure, preference, 'Costs this reading imposes on literal-identity communities if adopted.').

omega_variable(
    scholar_adoption_feasibility,
    'Can scholars adopt this reading from inside traditional institutions at survivable cost, or does the reading''s viability require institutional capture it cannot achieve while its advocates are expelled?',
    'Longitudinal tracking of reformist scholars'' institutional standing: promotion outcomes, exclusion from fatwa councils, innovation/deviance proceedings, and whether any traditional academy has ever licensed abrogation-based gender rulings.',
    'Prohibitive adoption costs would keep the reading structurally marginal regardless of textual merit; persistence analysis would shift from textual validity to institutional access, and the reading''s long-run effect would route through state legislatures rather than the academies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholar_adoption_feasibility, empirical, 'Exit-cost structure facing this reading''s advocates inside traditional institutions.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression sustaining compliance with the gender rulings structural (family and legal sanction, exit costs) or internalized (settled belief that the differential is divinely ordained and just)?',
    'Post-exit trajectory: compare women who emigrate or leave the tradition with those who remain under weakened enforcement - if acceptance of the differential persists where the sanction machinery is absent, a large internalized component exists.',
    'Internalized suppression raises effective suppression above the structural measure and predicts the arrangement surviving formal legal reform; purely structural suppression predicts rapid relaxation once sanction lifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the suppression holding the arrangement in place.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.18).
narrative_ontology:measurement(qura_tr_t10, quranic_gender_verses__progressive_abrogation, theater_ratio, 10, 0.22).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__progressive_abrogation, theater_ratio, 20, 0.26).
narrative_ontology:measurement(qura_tr_t30, quranic_gender_verses__progressive_abrogation, theater_ratio, 30, 0.3).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.33).
narrative_ontology:measurement(qura_tr_t50, quranic_gender_verses__progressive_abrogation, theater_ratio, 50, 0.35).
narrative_ontology:measurement(qura_tr_t60, quranic_gender_verses__progressive_abrogation, theater_ratio, 60, 0.36).
narrative_ontology:measurement(qura_tr_t70, quranic_gender_verses__progressive_abrogation, theater_ratio, 70, 0.38).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(qura_be_t10, quranic_gender_verses__progressive_abrogation, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__progressive_abrogation, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(qura_be_t30, quranic_gender_verses__progressive_abrogation, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(qura_be_t50, quranic_gender_verses__progressive_abrogation, base_extractiveness, 50, 0.86).
narrative_ontology:measurement(qura_be_t60, quranic_gender_verses__progressive_abrogation, base_extractiveness, 60, 0.87).
narrative_ontology:measurement(qura_be_t70, quranic_gender_verses__progressive_abrogation, base_extractiveness, 70, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(qura_su_t10, quranic_gender_verses__progressive_abrogation, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__progressive_abrogation, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(qura_su_t30, quranic_gender_verses__progressive_abrogation, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(qura_su_t50, quranic_gender_verses__progressive_abrogation, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(qura_su_t60, quranic_gender_verses__progressive_abrogation, suppression_requirement, 60, 0.77).
narrative_ontology:measurement(qura_su_t70, quranic_gender_verses__progressive_abrogation, suppression_requirement, 70, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, resource_allocation).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the Qur'an says about gender' decomposes into three structurally distinct constraints - one per reading of the kernel quranic_gender_verses. Each has its own epsilon (reading-indexed over the shared referent of the standing enforcement arrangement), its own beneficiary/victim emphasis, its own failure modes, and its own axioms. This member (progressive_abrogation) is distinguished by making the arrangement's coordination justification FALSE as its central premise (the rules are superseded), which pushes its assessment toward the extractive pole; literal_hierarchical accepts the justification (lower epsilon, same victims); contextual_egalitarian reframes it (verses retain contextual normativity). The members are linked pairwise via affects_constraints; contamination propagates through shared proof-texts (49:13, the chronology of surahs 2 and 4) and through the shared enforcement apparatus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
