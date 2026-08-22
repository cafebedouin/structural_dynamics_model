% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Originalist Reading: Equality Bounded by the Founding Social Taxonomy
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The Declaration's equality sentence, as absorbed into the constitutional
 *   order, is a contested kernel; this file instantiates the originalist
 *   reading - the arrangement under which the guarantee's scope is fixed by
 *   the founding generation's social taxonomy and founders' intent governs
 *   every subsequent scope question. The standing arrangement under contest
 *   is the constitutional order in which equal standing is distributed
 *   according to 1787 membership categories, periodically re-bounded through
 *   era-intent readings of each later amendment. The reading's own
 *   descriptive concessions fix the extraction structure: it concedes exactly
 *   who is inside and who is outside, and disputes the evaluation of that
 *   structure, not its existence - so epsilon is authored high against the
 *   standing arrangement, per the reading's own lights. Per the
 *   epsilon-invariance principle, the colloquial label decomposes into three
 *   structurally distinct constraints (this reading, the universalist
 *   reading, the textualist-paradox reading), each with its own epsilon,
 *   victim set, and file; they are linked through
 *   network.affects_constraints. Claimed type and metrics are authored
 *   independently: snare is the structural claim; the metric values are the
 *   descriptive record.
 *
 * KEY AGENTS:
 *   - founding_descendant_class: primary beneficiary (powerful/arbitrage) - inherits protected standing and interpretive authority
 *   - originalist_judicial_apparatus: agenda setter (institutional/arbitrage) - administers the intent-selection machinery
 *   - descendants_of_the_enslaved: primary target (moderate/constrained) - bears the bounded scope's central cost
 *   - women_outside_founders_taxonomy: target (organized/constrained) - excluded from the founding category, reached only through era-bounded amendments
 *   - indigenous_nations: target (organized/trapped) - outside the compact's taxonomy entirely, territory-locked
 *   - abolitionist_and_suffragist_tradition: excluded voice (organized/identity_locked) - held the universal reading for two centuries, never seated
 *   - constitutional_historians: analytical observer (analytical/analytical) - the archive cuts both ways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.74).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.62).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, snare).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Originalist Reading: Equality Bounded by the Founding Social Taxonomy").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '0d6d0a47-e33b-4074-8c8a-092ad6686084').
narrative_ontology:cs_kernel_codification('0d6d0a47-e33b-4074-8c8a-092ad6686084', fixed_text).
narrative_ontology:cs_authority_grounding('0d6d0a47-e33b-4074-8c8a-092ad6686084', lineage).
narrative_ontology:cs_interpretation_layer_present('0d6d0a47-e33b-4074-8c8a-092ad6686084').
narrative_ontology:cs_reading_relation('0d6d0a47-e33b-4074-8c8a-092ad6686084', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d6d0a47-e33b-4074-8c8a-092ad6686084', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('0d6d0a47-e33b-4074-8c8a-092ad6686084', foundational, founder_intent_fixes_equality_scope).
narrative_ontology:cs_axiom_status(founder_intent_fixes_equality_scope, holdable).
narrative_ontology:cs_axiom_grounding('0d6d0a47-e33b-4074-8c8a-092ad6686084', founder_intent_fixes_equality_scope, conventional).
narrative_ontology:cs_axiom('0d6d0a47-e33b-4074-8c8a-092ad6686084', secondary, period_intent_bounds_amendment_meaning).
narrative_ontology:cs_axiom_status(period_intent_bounds_amendment_meaning, holdable).
narrative_ontology:cs_axiom_grounding('0d6d0a47-e33b-4074-8c8a-092ad6686084', period_intent_bounds_amendment_meaning, conventional).
narrative_ontology:cs_reference_frame('0d6d0a47-e33b-4074-8c8a-092ad6686084', founding_taxonomic_settlement).
narrative_ontology:cs_drift_state('0d6d0a47-e33b-4074-8c8a-092ad6686084', post_reconstruction_civil_rights_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('0d6d0a47-e33b-4074-8c8a-092ad6686084', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_descendant_class).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, slaveholding_plantation_elite).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_legal_movement).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, descendants_of_the_enslaved).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women_outside_founders_taxonomy).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_judicial_apparatus).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, founder_intent_authority_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, anti_expansion_judicial_restraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Descendants of the founding settler and planter classes hold standing, wealth, and interpretive authority inherited from the founding settlement. Their equal standing is the settled baseline: their claims are always justiciable, and their position is the reference point against which other claims are measured. They bear none of the arrangement's burdens and can adopt or drop the founding-fidelity framing at will; exiting its benefits is not something they seek.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_descendant_class, beneficiary,
    powerful, generational, arbitrage, national).

% Jurists, law schools, clerkship networks, and movement institutions that administer the reading: they decide which founding-era materials count as evidence of intent, train the next generation of judges in the method, and staff the benches that apply it. Authority, careers, and institutional endowments flow from controlling the method. If they abandoned it tomorrow, nothing binds them personally - they would reposition within the same institutions.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_judicial_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__originalist_reading, originalist_judicial_apparatus, beneficiary).

% Their ancestors were counted as property or as non-members by the taxonomy that fixes the guarantee's scope, and redress for that counting runs through amendment-level action or through period-intent readings that reproduce the original exclusions. Formal citizenship came by amendment, but the method reads even that amendment through its own era's bounded understandings. Leaving the constitutional order is not a realistic option; exit means emigration at ruinous personal cost.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, descendants_of_the_enslaved, payer,
    moderate, generational, constrained, national).

% They were outside the founding category 'men' and remain outside the guarantee wherever the governing intent is the founders'. Where later amendment text reaches them, its scope is fixed by the ratifying era's understanding, which was itself restrictive. Organized advocacy is substantial - the suffrage movement and its successors are among the oldest continuous constitutional campaigns - but organization purchases influence, not exit: there is no jurisdictional or doctrinal exit from the intent-bound method.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women_outside_founders_taxonomy, payer,
    organized, biographical, constrained, national).

% They stand outside the founding social taxonomy altogether: the founding generation treated them as nations to be treaty-subordinated rather than members of the compact. Sovereignty claims are measured against the intents of governments that placed them outside the taxonomy, and the landmark formulations of their status were themselves applications of that measuring. Their homelands sit inside the republic's borders, making physical exit impossible; the arrangement travels with the territory.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_nations, payer,
    organized, generational, trapped, continental).

% From 1776 forward, Black and white abolitionists and later suffragists read the same sentence as a universal promise and demanded inclusion on its face. They were never seated in the founding conversation whose intent now governs, and the method assigns their centuries of interpretive labor no legal weight. Their commitment to the universal reading is constitutive of the tradition itself - abandoning it would dissolve the tradition - so they do not exit; they contest from outside the governing method.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, abolitionist_and_suffragist_tradition, excluded,
    organized, civilizational, identity_locked, national).

% They reconstruct founding-era usage, ratification records, and period understandings from the archive. Their findings sometimes corroborate the bounded reading and sometimes document broader contemporaneous understandings that cut against it. They hold no enforcement power, collect no rents from any outcome, and publish for audiences on every side of the dispute.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, founding_descendant_class).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, dispute-limiting rule for applying the Declaration's and Constitution's equality language: fix the scope at the founding generation's understanding, so officials and courts share one criterion, and route all widening disputes into the amendment process.
% TRANSFER_FUNCTION: Moves the protective force of the equality principle away from those outside the founding taxonomy and their descendants toward those inside it - withholding guaranteed equal standing from the former while securing the latter's inherited position - and moves interpretive authority to whoever controls the reconstruction of founding intent.
% ABSENT_VOICES: The abolitionist and suffragist traditions read the same sentence universally from the beginning and were never seated in the conversation whose intent now governs; contemporary communities whose claims the bounded reading renders non-justiciable likewise stand outside it. Their objections are documented in their own publications and in the congressional record of the eras they contested.
% DISAPPEARANCE_RATIONALE: If the bounded reading vanished overnight, equality claims currently foreclosed as non-justiciable would enter the courts, the descendant class's protected standing would become as contestable as anyone else's, and the interpretive apparatus's authority would collapse into open methodological competition - the distribution of equal standing, the legitimacy chain of judicial review, and the amendment-versus-interpretation division of labor would all reorganize.
% FOUNDING_PROBLEM: The founding settlement needed a public creed capable of binding thirteen fractious states while preserving the slave economy and the settlers' land regime: the equality sentence supplied the creed, and the social taxonomy supplied the quiet limit that kept the creed from dissolving the settlement.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the founding-era dissenting record itself - antislavery signers' objections, Frederick Douglass's 1852 address, and the Reconstruction framers' statements that the principle outran the founders' practice - attests both the founding problem and its contested status. The beneficiary set attests the opposite (that the creed remains necessary as founded). No disinterested arbiter exists; the corroboration is the documented dissent tradition spanning two centuries.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.74: the guarantee's protective force is withheld from everyone outside the founding taxonomy and from those whom era-bounded amendment readings continue to exclude, while the insider class's standing is secured as the baseline against which all claims are measured. Suppression 0.62 is raw and unscaled (only extractiveness is scaled by directionality and scope): persistence requires courts to refuse expansionist readings, standing doctrine to screen challengers, and supermajority amendment thresholds to channel all widening through processes the method's administrators influence. Theater 0.38: the scholarly apparatus is real (corpus linguistics, archival reconstruction), but a substantial share of activity is fidelity performance - practitioners themselves concede law-office history is endemic. Accessibility collapse 0.55: within committed circles alternatives collapse into lawlessness rhetoric, but living-constitutional, common-law, and textualist alternatives remain fully available in the broader profession, so collapse is partial and community-relative. Resistance 0.72: two centuries of continuous organized opposition, from the abolitionists through the civil-rights revolution to contemporary critical schools. The measurement series shares one eight-point grid (1790-2024) and is wave-shaped, not monotonic: consolidation (1790-1857), formal breach (1868), theatrical re-consolidation through era-bounded reading of the new text (1896), renewed breach (1954), methodological revival (1980-2024). The oscillation is partly the mechanism itself: each expansion of the guarantee is delivered, then re-bounded by intent-method - intermittent delivery sustains dependence on the method's administrators. Suppression_requirement is tracked because enforcement capacity visibly built and collapsed repeatedly: the fugitive-slave apparatus, Reconstruction enforcement and its gutting, the Jim Crow terror backstop, modern doctrinal policing. Coalition note: the payer seats are moderate and organized rather than powerless, and their coalition has twice broken the arrangement formally (1868, 1954) before re-consolidation - coalition power is real but has not held. Boltzmann note: identity_coordination is declared because the dominant function is boundary maintenance of the equal-citizenship class; the founding-covenant identity framing is partly cover for the withholding it performs, and the type's conservative floor correctly declines to excuse that coupling.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats compute a lawful-fidelity world: from inside the apparatus the arrangement is stability, determinacy, and democratic legitimacy, with the amendment process as the honest path for change. The payer seats compute enforced exclusion: the same determinacy is experienced as a lock, and the same amendment path as a door the method itself keeps barred, because era-bounded readings of each new amendment reproduce the old exclusions. Same-level divergence: women's organizations and tribal nations hold the same power atom but different exit profiles (constrained versus trapped) and different relations to the taxonomy (excluded would-be members of the compact versus peoples placed outside it entirely), so equal nominal power yields different directionalities - hence the organized-power override. The historian seat sees the archive authorize both the bounded and the broad contemporaneous understandings, which is why the dispute persists on evidence rather than resolving.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the descendant class receives secured standing (d near 0.0 - powerful, arbitrage exit); the plantation elite collected direct rents during the slavery era; the legal movement collects authority and career rents (secondary beneficiary, institutional, arbitrage). Payers: descendants of the enslaved (moderate power, constrained exit - high d), women outside the taxonomy (organized, constrained), indigenous nations (organized, trapped). Override rationale: a derivation keyed to power alone would read organized capacity as mitigating and lower d for the two organized payer seats; but the arrangement withholds the guarantee from these groups specifically, organization purchases influence without unlocking exit from the constitutional order, and organized challenge attracts intensified doctrinal policing - so organized-power d is overridden to 0.88. The apparatus sits near the beneficiary end through its secondary beneficiary role; the historian seat is analytical and feeds no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - a creed capable of binding thirteen states without dissolving the slave economy and the land regime - is contested: the union it preserved consolidated long ago, and the parties dispute whether anything remains for the arrangement to solve beyond inheritance protection. The mismatch consumer reads contested x world_rearranges: no dead-mandate flag fires, because the arrangement still actively rearranges the world daily (it decides justiciability). But the genealogy explains the shape: the coordinative half (a common creed) is spent, while the boundary-maintenance half persists because gains concentrate in the descendant class and fixing is prohibitive - supermajority amendment thresholds, life tenure, and methodological entrenchment. This is why the classification resists both mislabels: not a piton (gains are not diffuse; administration is active, not inertial), and not a rope (the coordination story cannot be separated from the withholding it performs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates only the originalist reading of the all_men_created_equal kernel; what victim set, beneficiary set, and epsilon would the universalist_reading and textualist_paradox_reading instantiate for the same text?',
    'Compile and classify the sibling stories independently, then diff the computed victim/beneficiary sets and effective extraction across the three readings.',
    'If the universalist reading computes near-zero extraction from the principle itself (extraction relocating to enforcement failures), the kernel''s extractive force is reading-relative rather than text-inherent; if all three compute high extraction, the founding-era record dominates whatever reading is adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this is one of three readings; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    intent_evidence_selection,
    'Whose understanding counts as founders'' intent - drafters, ratifiers, enacting coalitions, or general public meaning - and does the selection rule change the taxonomy''s boundaries?',
    'Systematic comparison of scope outcomes under drafter-intent versus ratifier-intent versus public-meaning corpora, including state ratification conventions elected on broader suffrage bases.',
    'Narrower selections concentrate the withheld guarantee on the excluded groups; broader public-meaning selections widen the guaranteed class somewhat without dissolving the bound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_evidence_selection, empirical, 'The selection rule for founding intent materially shifts the victim set.').

omega_variable(
    amendment_frame_reset,
    'Does the bounded reading treat Reconstruction-era amendments as resetting the reference frame (1868 intent superseding 1787 intent) or as additions permanently read through the founding frame?',
    'Doctrinal analysis of how period-intent method anchors the Fourteenth Amendment: to 1868 understandings (which licensed segregation) or to the 1787 frame.',
    'Frame-reset leaves open that the bound loosens across successive amendments; no-reset makes the excluded groups permanent targets regardless of textual change - the difference between a hardening arrangement and a potentially self-correcting one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_frame_reset, conceptual, 'Whether the reference frame is frozen at 1787 or resets at each amendment.').

omega_variable(
    beneficiary_descent_boundary,
    'Is the beneficiary class constituted by descent from the founding polity or by present adherence to the bounded reading - do newcomers who embrace the reading join the protected class?',
    'Track naturalization-and-adherence cases: whether immigrant adherents of the method receive the same interpretive deference and standing protection as descent-class members.',
    'A descent-constituted class makes the arrangement hereditary and hardens the extraction signature; an adherence-constituted class makes membership voluntary and shifts the structure toward coordinated boundary maintenance with willing insiders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_descent_boundary, conceptual, 'Boundary rule of the protected class: hereditary versus adherent.').

omega_variable(
    suppression_structural_internalized,
    'Is the measured suppression structural (court enforcement, standing doctrine, amendment-threshold barriers) or internalized (civic education transmitting the bounded reading as neutral rule-of-law)?',
    'Post-doctrinal-change trajectory: where the governing bench flips, does bounded-application behavior persist in lower courts and civic practice, indicating transmitted rather than enforced maintenance?',
    'If substantially internalized, effective suppression exceeds the structural measure and outlasts doctrinal reversal, placing the true suppression component above the authored scalar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized, empirical, 'Split between enforced and transmitted suppression mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1790, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1790, all_men_created_equal__originalist_reading, theater_ratio, 1790, 0.12).
narrative_ontology:measurement_basis(all__tr_t1790, observed).
narrative_ontology:measurement(all__tr_t1820, all_men_created_equal__originalist_reading, theater_ratio, 1820, 0.18).
narrative_ontology:measurement_basis(all__tr_t1820, observed).
narrative_ontology:measurement(all__tr_t1857, all_men_created_equal__originalist_reading, theater_ratio, 1857, 0.28).
narrative_ontology:measurement_basis(all__tr_t1857, observed).
narrative_ontology:measurement(all__tr_t1868, all_men_created_equal__originalist_reading, theater_ratio, 1868, 0.42).
narrative_ontology:measurement_basis(all__tr_t1868, observed).
narrative_ontology:measurement(all__tr_t1896, all_men_created_equal__originalist_reading, theater_ratio, 1896, 0.57).
narrative_ontology:measurement_basis(all__tr_t1896, observed).
narrative_ontology:measurement(all__tr_t1954, all_men_created_equal__originalist_reading, theater_ratio, 1954, 0.48).
narrative_ontology:measurement_basis(all__tr_t1954, observed).
narrative_ontology:measurement(all__tr_t1980, all_men_created_equal__originalist_reading, theater_ratio, 1980, 0.36).
narrative_ontology:measurement_basis(all__tr_t1980, observed).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__originalist_reading, theater_ratio, 2024, 0.38).
narrative_ontology:measurement_basis(all__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(all__be_t1790, all_men_created_equal__originalist_reading, base_extractiveness, 1790, 0.86).
narrative_ontology:measurement_basis(all__be_t1790, observed).
narrative_ontology:measurement(all__be_t1820, all_men_created_equal__originalist_reading, base_extractiveness, 1820, 0.88).
narrative_ontology:measurement_basis(all__be_t1820, observed).
narrative_ontology:measurement(all__be_t1857, all_men_created_equal__originalist_reading, base_extractiveness, 1857, 0.93).
narrative_ontology:measurement_basis(all__be_t1857, observed).
narrative_ontology:measurement(all__be_t1868, all_men_created_equal__originalist_reading, base_extractiveness, 1868, 0.58).
narrative_ontology:measurement_basis(all__be_t1868, observed).
narrative_ontology:measurement(all__be_t1896, all_men_created_equal__originalist_reading, base_extractiveness, 1896, 0.8).
narrative_ontology:measurement_basis(all__be_t1896, observed).
narrative_ontology:measurement(all__be_t1954, all_men_created_equal__originalist_reading, base_extractiveness, 1954, 0.52).
narrative_ontology:measurement_basis(all__be_t1954, observed).
narrative_ontology:measurement(all__be_t1980, all_men_created_equal__originalist_reading, base_extractiveness, 1980, 0.66).
narrative_ontology:measurement_basis(all__be_t1980, observed).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__originalist_reading, base_extractiveness, 2024, 0.74).
narrative_ontology:measurement_basis(all__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1790, all_men_created_equal__originalist_reading, suppression_requirement, 1790, 0.55).
narrative_ontology:measurement_basis(all__su_t1790, observed).
narrative_ontology:measurement(all__su_t1820, all_men_created_equal__originalist_reading, suppression_requirement, 1820, 0.62).
narrative_ontology:measurement_basis(all__su_t1820, observed).
narrative_ontology:measurement(all__su_t1857, all_men_created_equal__originalist_reading, suppression_requirement, 1857, 0.78).
narrative_ontology:measurement_basis(all__su_t1857, observed).
narrative_ontology:measurement(all__su_t1868, all_men_created_equal__originalist_reading, suppression_requirement, 1868, 0.6).
narrative_ontology:measurement_basis(all__su_t1868, observed).
narrative_ontology:measurement(all__su_t1896, all_men_created_equal__originalist_reading, suppression_requirement, 1896, 0.72).
narrative_ontology:measurement_basis(all__su_t1896, observed).
narrative_ontology:measurement(all__su_t1954, all_men_created_equal__originalist_reading, suppression_requirement, 1954, 0.58).
narrative_ontology:measurement_basis(all__su_t1954, observed).
narrative_ontology:measurement(all__su_t1980, all_men_created_equal__originalist_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement_basis(all__su_t1980, observed).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__originalist_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(all__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% The label 'all men are created equal' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the originalist reading (this file - bounded scope, intent-governed, high extraction onto historically excluded groups), the universalist reading (self-expanding principle; extraction relocates to enforcement gaps), and the textualist paradox reading (performative-contradiction diagnosis; a different failure mode entirely). The originalist reading is upstream historically: its two-century dominance produced the bounded-application record that the paradox reading critiques and the universalist reading opposes. Family members link bidirectionally through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
